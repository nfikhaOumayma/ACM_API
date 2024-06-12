/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.aop.logger;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Configuration;

/**
 * The {@link LoggerAfterProcessAspect} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Aspect
@Configuration
public class LoggerAfterProcessAspect {

	/** The logger. */
	private Logger logger = LoggerFactory.getLogger(this.getClass());

	/**
	 * Log after throwing all methods from service.
	 *
	 * @param joinPoint the join point
	 * @param error the error
	 */
	@AfterThrowing(pointcut = "execution(* com.acm.service.*.*(..))", throwing = "error")
	public void logAfterThrowingAllMethodsFromService(JoinPoint joinPoint, Throwable error) {

		logger.error(
				"############# Error has been occurred in Method : {} in service package #############",
				joinPoint.getSignature());
		logger.error("############# Exception : logAfterThrowingAllMethodsFromService() :  {}",
				error);
	}

	/**
	 * Log after throwing all methods from controller.
	 *
	 * @param joinPoint the join point
	 * @param error the error
	 */
	@AfterThrowing(pointcut = "execution(* com.acm.controller.*.*(..))", throwing = "error")
	public void logAfterThrowingAllMethodsFromController(JoinPoint joinPoint, Throwable error) {

		logger.error(
				"############# Error has been occurred in Method : {} in controller package #############",
				joinPoint.getSignature());
		logger.error("############# Exception : logAfterThrowingAllMethodsFromController() :  {}",
				error);
	}
}
