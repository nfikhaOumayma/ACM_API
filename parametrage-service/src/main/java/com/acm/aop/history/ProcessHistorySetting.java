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
 * {@link ProcessHistorySetting} @interface. we declare a new annotation named as
 * "@ProcessHistorySetting". Used to mark methods in which we call it.
 * 
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Target({java.lang.annotation.ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface ProcessHistorySetting {

	/**
	 * Action.
	 *
	 * @return the string
	 */
	String action() default CommonAOPConstants.NEW;
}
