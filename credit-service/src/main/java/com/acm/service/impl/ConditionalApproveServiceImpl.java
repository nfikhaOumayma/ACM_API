/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.repository.ConditionnalApproveRepository;
import com.acm.service.ConditionalApproveService;
import com.acm.service.NotificationsServices;
import com.acm.utils.dtos.AcmConditionnalApproveDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.AcmConditionnalApprove;
import com.acm.utils.models.Loan;
import com.acm.utils.models.QAcmConditionnalApprove;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ConditionalApproveServiceImpl } class.
 *
 * @author kouali
 * @since 0.1.0
 */
@Service
public class ConditionalApproveServiceImpl implements ConditionalApproveService {

	/** The acm conditionnal approve repository. */
	@Autowired
	private ConditionnalApproveRepository conditionnalApproveRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(CustomerLinksRelationshipServiceImpl.class);

	/**
	 * To entity.
	 *
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @return the acm conditionnal approve
	 */
	public AcmConditionnalApprove toEntity(AcmConditionnalApproveDTO acmConditionnalApproveDTO) {

		return mapper.map(acmConditionnalApproveDTO, AcmConditionnalApprove.class);
	}

	/**
	 * To dto.
	 *
	 * @param acmConditionnalApprove the acm conditionnal approve
	 * @return the acm conditionnal approve DTO
	 */
	public AcmConditionnalApproveDTO toDto(AcmConditionnalApprove acmConditionnalApprove) {

		return mapper.map(acmConditionnalApprove, AcmConditionnalApproveDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConditionalApproveService#create(java.util.List)
	 */
	@Override
	public List<AcmConditionnalApproveDTO> create(
			List<AcmConditionnalApproveDTO> acmConditionnalApproveDTOs) {

		List<AcmConditionnalApprove> acmConditionnalApproveEntity =
				new ArrayList<AcmConditionnalApprove>();
		acmConditionnalApproveDTOs.forEach((item) -> {
			acmConditionnalApproveEntity.add((AcmConditionnalApprove) CommonFunctions
					.mapperToSave(toEntity(item), userClient, logger));
		});
		List<AcmConditionnalApprove> acmConditionnalApproveEntitys =
				conditionnalApproveRepository.saveAll(acmConditionnalApproveEntity);
		sendNotificationForContionnalApprove(acmConditionnalApproveDTOs);

		return acmConditionnalApproveEntitys.stream().map(item -> toDto(item))
				.collect(Collectors.toList());

	}

	/**
	 * Send notification for contionnal approve.
	 *
	 * @author kouali
	 * @param acmConditionnalApproveDTOs the acm conditionnal approve DT os
	 */
	private void sendNotificationForContionnalApprove(
			List<AcmConditionnalApproveDTO> acmConditionnalApproveDTOs) {

		String action = CommonConstants.ACM_NOTIFICATION_CONDITIONNAL_APPROVE;
		String actionDescription = "New Condditionnal Approve has been opened";
		acmConditionnalApproveDTOs.forEach(item -> {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(item.getUser().getLogin(),
							NotificationCategory.CONDITIONNAL_APPROVE.name(),
							NotificationType.INFO.name(), Boolean.TRUE, action, actionDescription,
							null, null));

		});

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConditionalApproveService#find(java.lang.Long)
	 */
	@Override
	public List<AcmConditionnalApproveDTO> find(LoanDTO loanDTO) {

		List<AcmConditionnalApprove> acmConditionnalApproveEntitys =
				conditionnalApproveRepository.findByLoan(mapper.map(loanDTO, Loan.class));
		return acmConditionnalApproveEntitys.stream().map(item -> toDto(item))
				.collect(Collectors.toList());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConditionalApproveService#update(com.acm.utils.dtos.
	 * AcmConditionnalApproveDTO)
	 */
	@Override
	public AcmConditionnalApproveDTO update(AcmConditionnalApproveDTO acmConditionnalApproveDTO) {

		AcmConditionnalApprove acmConditionnalApprove = (AcmConditionnalApprove) CommonFunctions
				.mapperToUpdate(mapper.map(acmConditionnalApproveDTO, AcmConditionnalApprove.class),
						userClient, logger);

		return mapper.map(conditionnalApproveRepository.save(acmConditionnalApprove),
				AcmConditionnalApproveDTO.class);

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConditionalApproveService#countByIdLoan(long)
	 */
	@Override
	public Long countByIdLoanAndConditionnalValidation(Long loanId) {

		return conditionnalApproveRepository
				.countByLoanAndConditionnalValidationIsNullOrIsFalse(loanId);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ConditionalApproveService#find(com.acm.utils.dtos.AcmConditionnalApproveDTO)
	 */
	@Override
	public List<AcmConditionnalApproveDTO> find(AcmConditionnalApproveDTO conditionnalApproveDTO) {

		Preconditions.checkNotNull(conditionnalApproveDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init qExceptionRequest
		QAcmConditionnalApprove qAcmConditionnalApprove =
				QAcmConditionnalApprove.acmConditionnalApprove;
		// build Predicate using given params
		BooleanBuilder predicate = buildQuery(conditionnalApproveDTO, qAcmConditionnalApprove);
		// init pageable params (page number / page size / sorting direction if exist)

		// QueryDSL using springDATA
		Iterable<AcmConditionnalApprove> result = conditionnalApproveRepository.findAll(predicate);
		List<AcmConditionnalApproveDTO> resultDTO = new ArrayList<>();
		result.forEach(item -> resultDTO.add(mapper.map(item, AcmConditionnalApproveDTO.class)));
		return resultDTO;
	}

	/**
	 * Builds the query.
	 *
	 * @param acmConditionnalApproveDTO the acm conditionnal approve DTO
	 * @param qAcmConditionnalApprove the q acm conditionnal approve
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(AcmConditionnalApproveDTO acmConditionnalApproveDTO,
			QAcmConditionnalApprove qAcmConditionnalApprove) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qAcmConditionnalApprove.enabled.eq(Boolean.TRUE));

		// find by statut
		if (!ACMValidationUtils.isNullOrEmpty(acmConditionnalApproveDTO.getLoan().getLoanId())) {
			predicate.and(qAcmConditionnalApprove.loan.idLoan
					.eq(acmConditionnalApproveDTO.getLoan().getLoanId()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(acmConditionnalApproveDTO.getItem())) {
			if (!ACMValidationUtils.isNullOrEmpty(acmConditionnalApproveDTO.getItem().getId())) {
				predicate.and(qAcmConditionnalApprove.item.id
						.eq(acmConditionnalApproveDTO.getItem().getId()));
			}
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConditionalApproveService#countByItem(java.lang.Long)
	 */
	@Override
	public Long countByItem(Long idItem) {

		return conditionnalApproveRepository
				.countByItemAndConditionnalValidationIsNullOrIsFalse(idItem);
	}

}
