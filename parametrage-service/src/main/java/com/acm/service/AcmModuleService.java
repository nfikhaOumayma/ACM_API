/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import com.acm.utils.models.AcmModule;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmModuleService} class.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
public interface AcmModuleService {

	/**
	 * Find all.
	 *
	 * @author kouali
	 * @param predicate the predicate
	 * @return the iterable
	 */
	Iterable<AcmModule> findAll(BooleanBuilder predicate);
}
