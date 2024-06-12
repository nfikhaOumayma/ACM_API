/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.AcmIhmValidatorRepository;
import com.acm.service.AcmIhmValidatorService;
import com.acm.utils.dtos.AcmIhmValidatorDTO;
import com.acm.utils.models.AcmIhmValidator;
import com.acm.utils.models.QAcmIhmValidator;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmIhmValidatorServiceImpl} Class Impl.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class AcmIhmValidatorServiceImpl implements AcmIhmValidatorService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmIhmValidatorServiceImpl.class);

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private AcmIhmValidatorRepository acmIhmValidatorRepository;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmValidatorService#find(com.acm.utils.dtos.AcmIhmValidatorDTO)
	 */
	@Override
	public List<AcmIhmValidatorDTO> find(AcmIhmValidatorDTO acmIhmValidatorDTO) {

		// init QAcmEnvironnement
		QAcmIhmValidator qAcmIhmValidator = QAcmIhmValidator.acmIhmValidator;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmIhmValidator.enabled.eq(Boolean.TRUE));

		// find by key starting with given params
		if (!ACMValidationUtils.isNullOrEmpty(acmIhmValidatorDTO.getCodeValidator())) {
			predicate.and(qAcmIhmValidator.codeValidator.eq(acmIhmValidatorDTO.getCodeValidator()));
		}

		// QueryDSL using springDATA
		Iterable<AcmIhmValidator> iterable = acmIhmValidatorRepository.findAll(predicate);
		List<AcmIhmValidator> acmIhmValidators = new ArrayList<>();
		iterable.forEach(acmIhmValidators::add);
		logger.debug("{} : params was founded", acmIhmValidators.size());

		// mapping returned list
		List<AcmIhmValidatorDTO> acmIhmValidatorDTOs = new ArrayList<>();
		acmIhmValidators.forEach(acmIhmValidator -> acmIhmValidatorDTOs
				.add(mapper.map(acmIhmValidator, AcmIhmValidatorDTO.class)));

		logger.debug("Returning founded data ...");
		return acmIhmValidatorDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmIhmValidatorService#findById(java.lang.Long)
	 */
	@Override
	public AcmIhmValidatorDTO findById(Long id) {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.debug("Find AcmIhmValidator by Key : {}", id);
		AcmIhmValidator acmIhmValidator = acmIhmValidatorRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmIhmValidator)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmIhmValidator.class.getSimpleName());
			logger.error("{} {} {} {}", environment.getProperty("exception.message.not.found"),
					AcmIhmValidator.class.getSimpleName(), CommonExceptionsMessage.WITH_ID, id);
			return null;
		}
		logger.debug("AcmIhmValidator value : {}", id);
		return mapper.map(acmIhmValidator, AcmIhmValidatorDTO.class);
	}

}
