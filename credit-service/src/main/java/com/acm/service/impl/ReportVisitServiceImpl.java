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

import com.acm.aop.history.ProcessHistoryLoan;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ReportVisitRepository;
import com.acm.service.ReportVisitService;
import com.acm.utils.dtos.ReportVisitDTO;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QReportVisit;
import com.acm.utils.models.ReportVisit;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ReportVisitServiceImpl} ReportVisitServiceImpl.
 *
 * @author YesserSOmai
 * @since 0.2.0
 */
@Service
public class ReportVisitServiceImpl implements ReportVisitService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ReportVisitServiceImpl.class);

	/** The acmEnvironnement repository. */
	@Autowired
	private ReportVisitRepository acmReportVisitRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportVisitService#find(java.lang.Long)
	 */
	@Override
	public ReportVisitDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find ReportVisitService by ID : {}", id);
		ReportVisit acmReportVisit = acmReportVisitRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(acmReportVisit)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ReportVisit.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ ReportVisit.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ id);
		}
		return mapper.map(acmReportVisit, ReportVisitDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportVisitService#find(com.acm.utils.dtos.ReportVisitDTO)
	 */
	@Override
	public List<ReportVisitDTO> find(ReportVisitDTO reportVisitDTO) {

		// init QReportVisit
		QReportVisit qReportVisit = QReportVisit.reportVisit;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		if (!ACMValidationUtils.isNullOrEmpty(reportVisitDTO.getIdLoan())) {
			predicate.and(qReportVisit.loan.eq(new Loan(reportVisitDTO.getIdLoan())));
		}

		Iterable<ReportVisit> iterable = acmReportVisitRepository.findAll(predicate);
		List<ReportVisit> reportVisits = new ArrayList<>();
		iterable.forEach(reportVisits::add);
		logger.info("{} : Loan was founded", reportVisits.size());

		List<ReportVisitDTO> reportVisitDTOs = new ArrayList<>();
		reportVisits
				.forEach(report -> reportVisitDTOs.add(mapper.map(report, ReportVisitDTO.class)));
		return reportVisitDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportVisitService#save(com.acm.utils.dtos.ReportVisitDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.SAVE_REPORT_VISIT)
	public ReportVisitDTO save(ReportVisitDTO acmReportVisitDTO) {

		Preconditions.checkNotNull(acmReportVisitDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		ReportVisit acmReportVisit = mapper.map(acmReportVisitDTO, ReportVisit.class);
		CommonFunctions.mapperToSave(acmReportVisit, userClient, logger);
		acmReportVisit.setLoan(new Loan(acmReportVisitDTO.getIdLoan()));
		ReportVisit newacmReportVisit = acmReportVisitRepository.save(acmReportVisit);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, ReportVisit.class.getSimpleName());
		return mapper.map(newacmReportVisit, ReportVisitDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportVisitService#save(java.lang.Long,
	 * com.acm.utils.dtos.ReportVisitDTO)
	 */
	@Override
	public ReportVisitDTO save(Long id, ReportVisitDTO acmReportVisitDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(acmReportVisitDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update loan  with ID = {}", id);
		ReportVisit oldAcmReportVisit = acmReportVisitRepository.findById(id).orElse(null);

		// historique TODO

		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(oldAcmReportVisit)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, ReportVisit.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ReportVisit.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAcmReportVisit)
		mapper.map(acmReportVisitDTO, oldAcmReportVisit);
		CommonFunctions.mapperToUpdate(oldAcmReportVisit, userClient, logger);
		ReportVisit newAcmReportVisit = acmReportVisitRepository.save(oldAcmReportVisit);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ReportVisit.class.getSimpleName());
		return mapper.map(newAcmReportVisit, ReportVisitDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ReportVisitService#delete(com.acm.utils.dtos.ReportVisitDTO)
	 */
	@Override
	public void delete(ReportVisitDTO acmReportVisitDTO) {

		Preconditions.checkNotNull(acmReportVisitDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(acmReportVisitDTO.getIdReportVisit(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.warn("delete ReportVisit  with ID = {}", acmReportVisitDTO.getIdReportVisit());
		// delete object by id
		acmReportVisitRepository.deleteById(acmReportVisitDTO.getIdReportVisit());
		logger.info(CommonLoggerMessage.SUCCESFULL_DELETE, ReportVisitDTO.class.getSimpleName());
	}
}
