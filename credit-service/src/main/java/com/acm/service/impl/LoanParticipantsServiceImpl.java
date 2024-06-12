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
import com.acm.repository.LoanParticipantsRepository;
import com.acm.service.LoanParticipantsService;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.models.LoanParticipants;
import com.acm.utils.models.QLoanParticipants;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link LoanParticipantsServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.10.0
 */
@Service
public class LoanParticipantsServiceImpl implements LoanParticipantsService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(LoanParticipantsServiceImpl.class);

	/** The loanParticipants repository. */
	@Autowired
	private LoanParticipantsRepository loanParticipantsRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanParticipantsService#find(com.acm.utils.dtos.LoanParticipantsDTO)
	 */
	@Override
	public List<LoanParticipantsDTO> find(LoanParticipantsDTO loanParticipantsDTO) {

		// init Predicate
		BooleanBuilder predicate = buildQuery(loanParticipantsDTO);

		// QueryDSL using springDATA
		Iterable<LoanParticipants> iterable = loanParticipantsRepository.findAll(predicate);
		List<LoanParticipants> loanParticipantss = new ArrayList<>();
		iterable.forEach(loanParticipantss::add);
		logger.info("{} : LoanParticipants was founded", loanParticipantss.size());

		// mapping returned list
		List<LoanParticipantsDTO> loanParticipantsDTOs = new ArrayList<>();
		loanParticipantss.forEach(loanParticipants -> loanParticipantsDTOs
				.add(mapper.map(loanParticipants, LoanParticipantsDTO.class)));

		logger.info("Returning founded data ...");
		return loanParticipantsDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @author HaythemBenizid
	 * @param loanParticipantsDTO the loan participants DTO
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(LoanParticipantsDTO loanParticipantsDTO) {

		// init QLoanParticipants
		QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only ENABLED data
		predicate.and(qLoanParticipants.enabled.eq(Boolean.TRUE));

		// find by ID_LOAN if exist
		if (loanParticipantsDTO.getIdLoan() != null) {
			predicate.and(qLoanParticipants.idLoan.eq(loanParticipantsDTO.getIdLoan()));
		}

		// find by given USERNAME if exist
		if (loanParticipantsDTO.getUsername() != null) {
			predicate.and(qLoanParticipants.username.eq(loanParticipantsDTO.getUsername()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanParticipantsService#save(com.acm.utils.dtos.LoanParticipantsDTO)
	 */
	@Override
	public LoanParticipantsDTO save(LoanParticipantsDTO loanParticipantsDTO) {

		Preconditions.checkNotNull(loanParticipantsDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Check ID_LOAN && USERNAME is not null
		// Check if no record in DB with the given ID_LOAN && USERNAME
		if (loanParticipantsDTO.getIdLoan() != null && loanParticipantsDTO.getUsername() != null
				&& loanParticipantsRepository
						.findByIdLoanAndUsernameAndEnabled(loanParticipantsDTO.getIdLoan(),
								loanParticipantsDTO.getUsername(), Boolean.TRUE)
						.isEmpty()) {
			LoanParticipants loanParticipants =
					mapper.map(loanParticipantsDTO, LoanParticipants.class);
			CommonFunctions.mapperToSave(loanParticipants, userClient, logger);
			loanParticipants.setDateDebut(new Date());

			LoanParticipants newLoanParticipants =
					loanParticipantsRepository.save(loanParticipants);

			logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, newLoanParticipants);
			return mapper.map(newLoanParticipants, LoanParticipantsDTO.class);
		}
		return new LoanParticipantsDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanParticipantsService#update(com.acm.utils.dtos.LoanParticipants DTO)
	 */
	@Override
	public LoanParticipantsDTO update(LoanParticipantsDTO loanParticipantsDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(loanParticipantsDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanParticipantsDTO.getUsername(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(loanParticipantsDTO.getIdLoan(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init Predicate
		QLoanParticipants qLoanParticipants = QLoanParticipants.loanParticipants;
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qLoanParticipants.enabled.eq(Boolean.TRUE));
		// find by owner
		predicate.and(qLoanParticipants.username.eq(loanParticipantsDTO.getUsername()));
		// find by loan
		predicate.and(qLoanParticipants.idLoan.eq(loanParticipantsDTO.getIdLoan()));
		// find only if DATE_FIN is null
		// predicate.and(qLoanParticipants.dateFin.isNull())

		// find data
		Iterable<LoanParticipants> iterable = loanParticipantsRepository.findAll(predicate);
		List<LoanParticipants> loanParticipants = new ArrayList<>();
		iterable.forEach(loanParticipants::add);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(loanParticipants)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					LoanParticipants.class.getSimpleName());
			return save(loanParticipantsDTO);
		}
		// update all founded Data
		for (LoanParticipants oldLoanParticipants : loanParticipants) {
			CommonFunctions.mapperToUpdate(oldLoanParticipants, userClient, logger);
			oldLoanParticipants.setDateFin(new Date());
			LoanParticipants newLoanParticipants =
					loanParticipantsRepository.save(oldLoanParticipants);
			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, newLoanParticipants);
		}
		return loanParticipantsDTO;
	}
}
