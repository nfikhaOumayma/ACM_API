/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.aop.history;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.acm.constants.common.CommonAOPConstants;

/**
 * {@link ProcessHistoryLoan} @interface. we declare a new annotation named as
 * "@ProcessHistoryLoan". Used to mark methods in which after calling it.
 * 
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Target({java.lang.annotation.ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ProcessHistoryLoan {

	/**
	 * Action.
	 *
	 * @return the string
	 */
	String action() default CommonAOPConstants.SAVE_LOAN;
}
