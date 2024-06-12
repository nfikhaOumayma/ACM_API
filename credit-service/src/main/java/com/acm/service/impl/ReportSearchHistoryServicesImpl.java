/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ReportSearchHistoryRepository;
import com.acm.service.ReportSearchHistoryServices;
import com.acm.utils.dtos.ReportSearchHistoryDTO;
import com.acm.utils.dtos.ReportingDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.ReportSearchHistory;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link ReportSearchHistoryServicesImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Service
public class ReportSearchHistoryServicesImpl implements ReportSearchHistoryServices {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(ReportSearchHistoryServicesImpl.class);

	/** The reportSearchHistory repository. */
	@Autowired
	private ReportSearchHistoryRepository reportSearchHistoryRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ReportSearchHistoryServices#save(com.acm.utils.dtos.ReportSearchHistoryDTO)
	 */
	@Override
	public ReportSearchHistoryDTO save(ReportSearchHistoryDTO reportSearchHistoryDTO) {

		Preconditions.checkNotNull(reportSearchHistoryDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(reportSearchHistoryDTO.getReportingDTO())) {
			logger.warn("NO DATA to be stored");
			return reportSearchHistoryDTO;
		}
		// mapping data
		ReportSearchHistory reportSearchHistory =
				mapper.map(reportSearchHistoryDTO, ReportSearchHistory.class);
		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// setting username
		reportSearchHistory.setUsername(connectedUser.getLogin());
		reportSearchHistory.setInsertBy(connectedUser.getFullName());
		reportSearchHistory.setAcmVersion(0);
		reportSearchHistory.setDateInsertion(new Date());
		reportSearchHistory.setEnabled(Boolean.TRUE);
		// convert DATA to JSON
		reportSearchHistory.setSearchDataJson(CommonFunctions
				.convertObjectToJSONString(reportSearchHistoryDTO.getReportingDTO()));
		// saving new data
		ReportSearchHistory newReportSearchHistory =
				reportSearchHistoryRepository.save(reportSearchHistory);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				ReportSearchHistory.class.getSimpleName());
		return mapper.map(newReportSearchHistory, ReportSearchHistoryDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportSearchHistoryServices#save(java.lang.Long,
	 * com.acm.utils.dtos.ReportSearchHistoryDTO)
	 */
	@Override
	public ReportSearchHistoryDTO save(Long id, ReportSearchHistoryDTO reportSearchHistoryDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(reportSearchHistoryDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Update ReportSearchHistory with ID = {}", id);
		if (ACMValidationUtils.isNullOrEmpty(reportSearchHistoryDTO.getReportingDTO())) {
			logger.warn("NO DATA to be stored");
			return reportSearchHistoryDTO;
		}
		ReportSearchHistory oldReportSearchHistory =
				reportSearchHistoryRepository.findById(id).orElse(null);
		// check if object is null
		if (oldReportSearchHistory == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ReportSearchHistory.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ReportSearchHistory.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping data
		CommonFunctions.mapperToUpdate(oldReportSearchHistory, userClient, logger);

		// convert DATA to JSON
		oldReportSearchHistory.setSearchDataJson(CommonFunctions
				.convertObjectToJSONString(reportSearchHistoryDTO.getReportingDTO()));
		ReportSearchHistory newReportSearchHistory =
				reportSearchHistoryRepository.save(oldReportSearchHistory);
		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE,
				ReportSearchHistory.class.getSimpleName());
		return mapper.map(newReportSearchHistory, ReportSearchHistoryDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportSearchHistoryServices#find()
	 */
	@Override
	public List<ReportSearchHistoryDTO> find(ReportSearchHistoryDTO reportSearchHistoryDTO) {

		// return empty list if no report was selected
		if (ACMValidationUtils.isNullOrEmpty(reportSearchHistoryDTO)
				|| ACMValidationUtils.isNullOrEmpty(reportSearchHistoryDTO.getReportName())) {
			return new ArrayList<>();
		}
		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// find search data by connected user and report name
		List<ReportSearchHistory> reportSearchHistory = reportSearchHistoryRepository
				.findByReportNameAndUsernameAndEnabled(reportSearchHistoryDTO.getReportName(),
						connectedUser.getLogin(), Boolean.TRUE);
		// mapping data
		List<ReportSearchHistoryDTO> reportSearchHistoryDTOs = new ArrayList<>();
		reportSearchHistory.forEach(notification -> {
			ReportSearchHistoryDTO dto = mapper.map(notification, ReportSearchHistoryDTO.class);
			// parsing JSON to Object
			ReportingDTO reportingDTO = (ReportingDTO) CommonFunctions
					.convertJSONStringtoObject(dto.getSearchDataJson(), ReportingDTO.class);
			dto.setReportingDTO(reportingDTO);
			reportSearchHistoryDTOs.add(dto);
		});
		logger.info("Returning {} ReportSearchHistory", reportSearchHistoryDTOs.size());
		return reportSearchHistoryDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ReportSearchHistoryService#delete(com.acm.utils.dtos.ReportSearchHistoryDTO)
	 */
	@Override
	public void delete(ReportSearchHistoryDTO reportSearchHistoryDTO) {

		Preconditions.checkNotNull(reportSearchHistoryDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(reportSearchHistoryDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete reportSearchHistory  with ID = {}", reportSearchHistoryDTO.getId());
		// delete object by id
		reportSearchHistoryRepository.deleteById(reportSearchHistoryDTO.getId());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE,
				ReportSearchHistory.class.getSimpleName());
	}
}
