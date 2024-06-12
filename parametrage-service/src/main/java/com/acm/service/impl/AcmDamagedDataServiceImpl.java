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
import com.acm.repository.AcmDamagedDataRepository;
import com.acm.service.AcmDamageDataService;
import com.acm.utils.dtos.AcmDamagedDataDTO;
import com.acm.utils.models.AcmDamagedData;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link AcmDamagedDataServiceImpl} class.
 *
 * @author mlamloum
 * @since 0.1.0
 */
@Service
public class AcmDamagedDataServiceImpl implements AcmDamageDataService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AcmDamagedDataServiceImpl.class);

	/** The acm damaged customer repository. */
	@Autowired
	AcmDamagedDataRepository acmDamagedCustomerRepository;
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
	public AcmDamagedDataDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Damaged Customer by ID : {}", id);
		AcmDamagedData acmDamagedCustomer = acmDamagedCustomerRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmDamagedCustomer)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					AcmDamagedData.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ AcmDamagedData.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(acmDamagedCustomer, AcmDamagedDataDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AcmDamageDataService#save(com.acm.utils.dtos.AcmDamagedDataDTO)
	 */
	@Override
	public AcmDamagedDataDTO save(AcmDamagedDataDTO acmDamagedCustomerDTO) {

		Preconditions.checkNotNull(acmDamagedCustomerDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		AcmDamagedData acmDamagedCustomer = mapper.map(acmDamagedCustomerDTO, AcmDamagedData.class);
		CommonFunctions.mapperToSave(acmDamagedCustomer, userClient, logger);
		AcmDamagedData newAcmDamagedCustomer =
				acmDamagedCustomerRepository.save(acmDamagedCustomer);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmDamagedData.class.getSimpleName());
		return mapper.map(newAcmDamagedCustomer, AcmDamagedDataDTO.class);
	}

}
