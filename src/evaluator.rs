use crate::ast::*;
use crate::lexer::Token;
use crate::object::{self, Object};

type EvalError = String;
type EvalResult = Result<Object, EvalError>;

pub fn eval(node: &Node, env: &mut object::Environment) -> EvalResult {
    match node {
        Node::Program(prog) => eval_program(prog, env),
        Node::Statement(stmt) => eval_statement(stmt, env),
        Node::Expression(exp) => eval_expression(exp, env),
    }
}

fn eval_block(block: &Vec<Statement>, env: &mut object::Environment) -> EvalResult {
    let mut result = Object::Null;
    for statement in block {
        let res = eval_statement(statement, env)?;
        match res {
            Object::Return(_) => return Ok(res),
            _ => result = res,
        }
    }
    Ok(result)
}

fn eval_program(program: &Program, env: &mut object::Environment) -> EvalResult {
    let mut result = Object::Null;
    for statement in &program.statements {
        result = eval_statement(statement, env)?;
        if let Object::Return(ret) = result {
            return Ok(ret.value);
        }
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: &mut object::Environment) -> EvalResult {
    match statement {
        Statement::Expression(exp) => eval_expression(&exp.expression, env),
        Statement::Let(stmt) => {
            let val = eval_expression(&stmt.value, env)?;
            env.set(stmt.name.clone(), val.clone());
            Ok(val)
        }
        Statement::Return(ret) => {
            let val = eval_expression(&ret.value, env)?;
            Ok(Object::Return(Box::new(object::Return { value: val })))
        }
    }
}

fn eval_minus_prefix_operator_expression(right: &Object) -> EvalResult {
    match right {
        Object::Int(int) => Ok(Object::Int(-int)),
        _ => Err(format!("Unknown operator: -{}", right)),
    }
}

fn eval_bang_operator_expression(right: &Object) -> EvalResult {
    match right {
        Object::Bool(b) => Ok(Object::Bool(!b)),
        // null is false -> !false => true
        Object::Null => Ok(Object::Bool(true)),
        _ => Ok(Object::Bool(false)),
    }
}

fn eval_prefix_expression(operator: &Token, right: &Object) -> EvalResult {
    match operator {
        Token::Bang => eval_bang_operator_expression(right),
        Token::Minus => eval_minus_prefix_operator_expression(right),
        _ => Err(format!("Unknown operator: {}{}", operator, right)),
    }
}

fn native_bool_to_boolean_object(b: bool) -> EvalResult {
    Ok(Object::Bool(b))
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> EvalResult {
    match operator {
        Token::Plus => Ok(Object::Int(left + right)),
        Token::Minus => Ok(Object::Int(left - right)),
        Token::Asterisk => Ok(Object::Int(left * right)),
        Token::Slash => Ok(Object::Int(left / right)),
        Token::Gt => native_bool_to_boolean_object(left > right),
        Token::Lt => native_bool_to_boolean_object(left < right),
        Token::Eq => native_bool_to_boolean_object(left == right),
        Token::Neq => native_bool_to_boolean_object(left != right),
        _ => Err(format!("Unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_bool_infix_expression(operator: &Token, left: bool, right: bool) -> EvalResult {
    match operator {
        Token::Eq => native_bool_to_boolean_object(left == right),
        Token::Neq => native_bool_to_boolean_object(left != right),
        _ => Err(format!("Invalid operator for booleans: {}", operator)),
    }
}

fn eval_infix_expression(operator: &Token, left: &Object, right: &Object) -> EvalResult {
    match (left, right) {
        (Object::Int(int_l), Object::Int(int_r)) => {
            eval_integer_infix_expression(operator, *int_l, *int_r)
        }
        (Object::Bool(b_l), Object::Bool(b_r)) => eval_bool_infix_expression(operator, *b_l, *b_r),
        _ => Err(format!(
            "Type missmatch: {:?} {} {:?}",
            left, operator, right
        )),
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Bool(b) => *b,
        Object::Null => false,
        Object::Int(_) => true,
        Object::Return(ret) => is_truthy(&ret.value),
    }
}

fn eval_if_expression(
    expression: &std::boxed::Box<IfExpression>,
    env: &mut object::Environment,
) -> EvalResult {
    let condition = eval_expression(&expression.condition, env)?;
    if is_truthy(&condition) {
        eval_block(&expression.consequence.statements, env)
    } else {
        match &expression.alternative {
            Some(block) => eval_block(&block.statements, env),
            None => Ok(Object::Null),
        }
    }
}

fn eval_identifier(identifier: String, env: &mut object::Environment) -> EvalResult {
    match env.get(&identifier) {
        Some(val) => Ok(val),
        None => Err(format!("Variable `{}` not found", identifier)),
    }
}

fn eval_expression(expression: &Expression, env: &mut object::Environment) -> EvalResult {
    match expression {
        Expression::Int(int) => Ok(Object::Int(*int)),
        Expression::Bool(b) => Ok(Object::Bool(*b)),
        Expression::Prefix(pre) => {
            eval_prefix_expression(&pre.operator, &eval_expression(&pre.right, env)?)
        }
        Expression::Infix(inf) => eval_infix_expression(
            &inf.operator,
            &eval_expression(&inf.left, env)?,
            &eval_expression(&inf.right, env)?,
        ),
        Expression::If(if_exp) => eval_if_expression(if_exp, env),
        Expression::Identifier(ident) => eval_identifier(ident.to_string(), env),
        _ => todo!(),
    }
}
