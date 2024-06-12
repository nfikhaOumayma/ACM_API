/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.repository.AcmModuleRepository;
import com.acm.service.AcmModuleService;
import com.acm.utils.models.AcmModule;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmModuleServiceImpl} Class Impl.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class AcmModuleServiceImpl implements AcmModuleService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmModuleServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private AcmModuleRepository acmModuleRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmModuleService#findAll(com.querydsl.core.BooleanBuilder)
	 */
	@Override
	public Iterable<AcmModule> findAll(BooleanBuilder predicate) {

		return acmModuleRepository.findAll(predicate);
	}

}
