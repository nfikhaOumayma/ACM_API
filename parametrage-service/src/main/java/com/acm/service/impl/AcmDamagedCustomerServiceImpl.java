/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmDamagedCustomerRepository;
import com.acm.service.AcmDamageCustomerService;
import com.acm.utils.dtos.AcmDamagedCustomerDTO;
import com.acm.utils.models.AcmDamagedCustomer;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * The Class AcmDamagedCustomerServiceImpl.
 */
@Service
public class AcmDamagedCustomerServiceImpl implements AcmDamageCustomerService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AcmDamagedCustomerServiceImpl.class);

	/** The acm damaged customer repository. */
	@Autowired
	AcmDamagedCustomerRepository acmDamagedCustomerRepository;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDamageCustomerService#find(java.lang.Long)
	 */
	@Override
	public AcmDamagedCustomerDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Damaged Customer by ID : {}", id);
		AcmDamagedCustomer acmDamagedCustomer =
				acmDamagedCustomerRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmDamagedCustomer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmDamagedCustomer.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AcmDamagedCustomer.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(acmDamagedCustomer, AcmDamagedCustomerDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDamageCustomerService#save(com.acm.utils.dtos.AcmDamagedCustomerDTO)
	 */
	@Override
	public AcmDamagedCustomerDTO save(AcmDamagedCustomerDTO acmDamagedCustomerDTO) {

		Preconditions.checkNotNull(acmDamagedCustomerDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmDamagedCustomer acmDamagedCustomer =
				mapper.map(acmDamagedCustomerDTO, AcmDamagedCustomer.class);
		CommonFunctions.mapperToSave(acmDamagedCustomer, userClient, logger);
		AcmDamagedCustomer newAcmDamagedCustomer =
				acmDamagedCustomerRepository.save(acmDamagedCustomer);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				AcmDamagedCustomer.class.getSimpleName());
		return mapper.map(newAcmDamagedCustomer, AcmDamagedCustomerDTO.class);
	}

}
