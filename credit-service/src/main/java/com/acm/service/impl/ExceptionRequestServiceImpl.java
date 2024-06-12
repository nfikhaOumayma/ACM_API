/**
 * Copyright(C)TALYS™-All Rights Reserved Unauthorized copying of this file,via any
 * medium/is*strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.RequestAlreadyExistException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ExceptionRequestRepository;
import com.acm.service.ExceptionRequestService;
import com.acm.service.NotificationsServices;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.ExceptionRequestCountDTO;
import com.acm.utils.dtos.ExceptionRequestDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.ExceptionRequestPaginationDTO;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.enums.UserCategory;
import com.acm.utils.models.AcmMezaCard;
import com.acm.utils.models.ExceptionRequest;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.QExceptionRequest;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link ExceptionRequestServiceImpl} class.
 *
 * @author ManelLamloum
 * @since 0.1.0
 */
@Service
public class ExceptionRequestServiceImpl implements ExceptionRequestService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ExceptionRequestServiceImpl.class);

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The exception request repository. */
	@Autowired
	private ExceptionRequestRepository exceptionRequestRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;
	/** The environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExceptionRequestService#find(com.acm.utils.dtos.ExceptionRequestDTO)
	 */
	@Override
	public List<ExceptionRequestDTO> find(ExceptionRequestDTO exceptionRequestDTO) {

		// init QGroupe
		QExceptionRequest qExceptionRequest = QExceptionRequest.exceptionRequest;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qExceptionRequest.enabled.eq(Boolean.TRUE));

		// find by ID
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getId())) {
			predicate.and(qExceptionRequest.id.eq(exceptionRequestDTO.getId()));
		}

		// find by STATUT
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getStatut())) {
			predicate.and(qExceptionRequest.statut.eq(exceptionRequestDTO.getStatut()));
		}

		// find by ID CUSTOMER
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getCustomerId())) {
			predicate.and(qExceptionRequest.customerId.eq(exceptionRequestDTO.getCustomerId()));
		}
		// find by statut in
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getListStatut())) {
			predicate.and(qExceptionRequest.statut.in(exceptionRequestDTO.getListStatut()));
		}
		// QueryDSL using springDATA
		Iterable<ExceptionRequest> iterable = exceptionRequestRepository.findAll(predicate);
		List<ExceptionRequest> exceptionRequests = new ArrayList<>();
		iterable.forEach(exceptionRequests::add);
		logger.info("{} : ExceptionRequest was founded", exceptionRequests.size());

		// mapping returned list
		List<ExceptionRequestDTO> exceptionRequestDTOs = new ArrayList<>();
		exceptionRequests.forEach(exceptionRequestParam -> exceptionRequestDTOs
				.add(mapper.map(exceptionRequestParam, ExceptionRequestDTO.class)));

		logger.info("Returning founded data ...");
		return exceptionRequestDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExceptionRequestService#find(com.acm.utils.dtos.ExceptionRequestDTO)
	 */
	@Override
	public ExceptionRequestPaginationDTO findPagination(
			ExceptionRequestPaginationDTO exceptionRequestPaginationDTO) {

		Preconditions.checkNotNull(exceptionRequestPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init qExceptionRequest
		QExceptionRequest qExceptionRequest = QExceptionRequest.exceptionRequest;
		// build Predicate using given params
		BooleanBuilder predicate =
				buildQuery(exceptionRequestPaginationDTO.getParams(), qExceptionRequest);
		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		// default sort by applyDate : DESC
		pageable = PageRequest.of(exceptionRequestPaginationDTO.getPageNumber(),
				exceptionRequestPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		// QueryDSL using springDATA
		Page<ExceptionRequest> pageResult = exceptionRequestRepository.findAll(predicate, pageable);
		if (pageResult.hasContent()) {
			List<ExceptionRequest> exceptionRequests = pageResult.getContent();
			logger.info(
					"find by pagination method : {} : Loan was founded (PageNumber = {} / PageSize = {} )",
					exceptionRequests.size(), exceptionRequestPaginationDTO.getPageNumber(),
					exceptionRequestPaginationDTO.getPageSize());
			List<ExceptionRequestDTO> exceptionRequestDTOs = new ArrayList<>();
			exceptionRequests.forEach(exceptionReq -> {
				ExceptionRequestDTO exceptionRequestDTOResultat =
						mapper.map(exceptionReq, ExceptionRequestDTO.class);
				exceptionRequestDTOs.add(exceptionRequestDTOResultat);
			});
			// setting data
			exceptionRequestPaginationDTO.setResult(exceptionRequestDTOs);
			// setting totals pages
			exceptionRequestPaginationDTO.setTotalElements(pageResult.getTotalElements());
			// setting totals elements
			exceptionRequestPaginationDTO.setTotalPages(pageResult.getTotalPages());
		}
		return exceptionRequestPaginationDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExceptionRequestService#save(com.acm.utils.dtos.ExceptionRequestDTO)
	 */
	@Override
	public ExceptionRequestDTO save(ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException, RequestAlreadyExistException {

		Preconditions.checkNotNull(exceptionRequestDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);

		// check if the customer already have an opened or accepted request exception then throw
		// exception
		List<Integer> listStatut = Arrays.asList(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.NEW_STATUT_REQUEST)
						.getKey(),
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ACCEPTED_STATUT_REQUEST)
						.getKey());
		List<ExceptionRequestDTO> ExceptionRequestDTOs =
				find(new ExceptionRequestDTO(exceptionRequestDTO.getCustomerId(), listStatut));
		if (!ACMValidationUtils.isNullOrEmpty(ExceptionRequestDTOs)) {
			throw new RequestAlreadyExistException(
					new ExceptionResponseMessage(CommonErrorCode.OPENED_REQUEST_ALREADY_EXIST,
							CommonExceptionsMessage.RENEWAL_CONDITION_SETTING_NOT_FOUND),
					CommonExceptionsMessage.RENEWAL_CONDITION_SETTING_NOT_FOUND);
		}
		assignExceptionRequest(exceptionRequestDTO);
		ExceptionRequest exceptionRequest = mapper.map(exceptionRequestDTO, ExceptionRequest.class);
		CommonFunctions.mapperToSave(exceptionRequest, userClient, logger);
		ExceptionRequest newExceptionRequest = exceptionRequestRepository.save(exceptionRequest);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AcmMezaCard.class.getSimpleName());
		return mapper.map(newExceptionRequest, ExceptionRequestDTO.class);

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExceptionRequestService#save(java.lang.Long,
	 * com.acm.utils.dtos.ExceptionRequestDTO)
	 */
	@Override
	public ExceptionRequestDTO save(Long id, ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(exceptionRequestDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update ExceptionRequest with ID = {}", id);
		ExceptionRequest oldExceptionRequest = exceptionRequestRepository.findById(id).orElse(null);
		// check if object is null
		if (oldExceptionRequest == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					ExceptionRequest.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + ExceptionRequest.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}

		// mapping new data with existing data (oldExceptionRequest)
		oldExceptionRequest.setStatut(exceptionRequestDTO.getStatut());
		oldExceptionRequest.setOwnerName(exceptionRequestDTO.getOwnerName());
		oldExceptionRequest.setOwnerUsername(exceptionRequestDTO.getOwnerUsername());
		oldExceptionRequest.setRejectNote(exceptionRequestDTO.getRejectNote());
		CommonFunctions.mapperToUpdate(oldExceptionRequest, userClient, logger);
		ExceptionRequest newExceptionRequest = exceptionRequestRepository.save(oldExceptionRequest);
		ExceptionRequestDTO newExceptionRequestDTO =
				mapper.map(newExceptionRequest, ExceptionRequestDTO.class);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, ExceptionRequest.class.getSimpleName());
		return newExceptionRequestDTO;

	}

	/**
	 * Assign exception request.
	 *
	 * @param exceptionRequestDTO the exception request DTO
	 * @return the exception request DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public ExceptionRequestDTO assignExceptionRequest(ExceptionRequestDTO exceptionRequestDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(exceptionRequestDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		String approveExceptionsGroupCode =
				parametrageClient.find(CommonConstants.APPROVE_EXCEPTIONS).getValue();
		// find group by code group
		GroupeDTO groupeDTO = parametrageClient.findGroupeByCode(approveExceptionsGroupCode);
		if (ACMValidationUtils.isNullOrEmpty(groupeDTO)) {
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Groupe.class.getSimpleName() + CommonExceptionsMessage.NOT_FOUND + " "
							+ CommonConstants.APPROVE_EXCEPTIONS + "with code "
							+ approveExceptionsGroupCode);
		}

		// get the list of users that have loanBranch in their accessBranch
		UserDTO userParam = new UserDTO();
		userParam.setGroupeCode(approveExceptionsGroupCode);
		List<UserDTO> userDTOParam = userClient.findByGroupe(userParam);
		// if the group has only one user( with loan branch IN his list of access branch) then
		// assign to this user
		if (userDTOParam.size() == 1) {
			exceptionRequestDTO.setOwnerUsername(userDTOParam.get(0).getLogin());
			exceptionRequestDTO.setOwnerName(userDTOParam.get(0).getFullName());
			// send notification to the owner
			String actionDescription = "New Exception Request has been created";
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(userDTOParam.get(0).getLogin(),
							NotificationCategory.REQUEST_EXCEPTION.name(),
							NotificationType.INFO.name(), Boolean.TRUE,
							CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_NEW,
							actionDescription, null, null));
			logger.info("New Exception Request Notification [{}] has been inserted.",
					notificationsDTO);
		}
		else {
			exceptionRequestDTO.setGroupOwnerCode(approveExceptionsGroupCode);
			// send notification to the group users
			notifyUsersPerGroup(approveExceptionsGroupCode);
		}
		return exceptionRequestDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.ExceptionRequestService#updateStatusAndAssignToUserConnected(com.acm.utils.
	 * dtos.ExceptionRequestDTO)
	 */
	@Override
	public ExceptionRequestDTO updateStatusAndAssignToUserConnected(
			ExceptionRequestDTO exceptionRequestDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(exceptionRequestDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(exceptionRequestDTO.getId(),
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// get connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// get the groupe of connected user
		Optional<GroupeDTO> groupeDTO = connectedUser.getGroupes().stream().findFirst();
		// get codeGroup allowed to update ExceptionRequests : 'APPROVE_EXCEPTIONS' value in
		// ACM_ENVIRONMENT table
		String approveExceptionsGroupCode =
				parametrageClient.find(CommonConstants.APPROVE_EXCEPTIONS).getValue();
		// if update to a status different than "Cancelled status" => check if the connected user
		// has the right to update the exceptionRequest ( check on value of 'APPROVE_EXCEPTIONS' in
		// ACM_ENVIRONMENT table)
		// else if update to cancelled status => check that the connected user is the maker of the
		// exceptionReques
		if ((exceptionRequestDTO.getStatut().equals(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED_STATUT_REQUEST).getKey())
				&& !connectedUser.getLogin().equals(exceptionRequestDTO.getMakerUsername()))
				|| (!exceptionRequestDTO.getStatut()
						.equals(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED_STATUT_REQUEST)
								.getKey())
						&& (ACMValidationUtils.isNullOrEmpty(approveExceptionsGroupCode)
								|| ACMValidationUtils.isNullOrEmpty(groupeDTO) || !groupeDTO.get()
										.getCode().equals(approveExceptionsGroupCode)))) {
			logger.error(CommonExceptionsMessage.GROUP_CONNECTED_NOT_ALLOWED);
			throw new ResourcesNotFoundException(
					CommonExceptionsMessage.GROUP_CONNECTED_NOT_ALLOWED);
		}

		// set the connected user as the owner of the Exception request
		exceptionRequestDTO.setOwnerName(connectedUser.getFullName());
		exceptionRequestDTO.setOwnerUsername(connectedUser.getLogin());
		// send Notif
		String action = "";
		if (exceptionRequestDTO.getStatut() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED_STATUT_REQUEST).getKey()) {
			action = CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_CANCELLED;
		}
		else if (exceptionRequestDTO.getStatut() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED_STATUT_REQUEST).getKey()) {
			action = CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_REJECTED;
		}
		else if (exceptionRequestDTO.getStatut() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.ACCEPTED_STATUT_REQUEST).getKey()) {
			action = CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_ACCEPTED;
		}
		String actionDescription = "New Exception Request has been updated";
		NotificationsDTO notificationsDTO = notificationsServices
				.save(new NotificationsDTO(exceptionRequestDTO.getMakerUsername(),
						NotificationCategory.REQUEST_EXCEPTION.name(), NotificationType.INFO.name(),
						Boolean.TRUE, action, actionDescription, null, null));
		logger.info("New Exception Request Notification [{}] has been updated.", notificationsDTO);
		// update
		return save(exceptionRequestDTO.getId(), exceptionRequestDTO);
	}

	/**
	 * Notify users per group.
	 *
	 * @author ManelLamloum
	 * @param groupeCode the groupe code
	 */
	private void notifyUsersPerGroup(String groupeCode) {

		String actionDescription = " New Exception Request has been created";
		// SEND NOTIFICATION TO GROUP OF USERS
		// find users by groupe code and access branches
		UserDTO userParam = new UserDTO();
		userParam.setGroupeCode(groupeCode);
		List<UserDTO> userDTOs = userClient.findByGroupe(userParam);
		if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {

			NotificationsDTO notificationsDTO = new NotificationsDTO();
			// list of users
			for (UserDTO userDTO : userDTOs) {
				notificationsDTO =
						notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
								NotificationCategory.REQUEST_EXCEPTION.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_EXCEPTION_REQUEST_NEW,
								actionDescription, null, null));
				logger.info("New Exception Request Notification [{}] has been inserted.",
						notificationsDTO);

			}
		}
	}

	/**
	 * Builds the query.
	 *
	 * @param exceptionRequestDTO the exception request DTO
	 * @param qExceptionRequest the q exception request
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(ExceptionRequestDTO exceptionRequestDTO,
			QExceptionRequest qExceptionRequest) {

		// find connected user details
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// get the groupe of connected user
		Optional<GroupeDTO> groupeDTO = connectedUser.getGroupes().stream().findFirst();
		String approveExceptionsGroupCode =
				parametrageClient.find(CommonConstants.APPROVE_EXCEPTIONS).getValue();
		if (ACMValidationUtils.isNullOrEmpty(approveExceptionsGroupCode)
				|| ACMValidationUtils.isNullOrEmpty(groupeDTO)
				|| !groupeDTO.get().getCode().equals(approveExceptionsGroupCode)) {

			// return only the ExceptionRequests created by the connected user.
			exceptionRequestDTO.setMakerUsername(connectedUser.getLogin());
			exceptionRequestDTO.setMakerName(connectedUser.getFullName());

		}

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qExceptionRequest.enabled.eq(Boolean.TRUE));

		// find by statut
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getStatut())) {
			predicate.and(qExceptionRequest.statut.eq(exceptionRequestDTO.getStatut()));
		}
		// find by customer name
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getCustomerName())) {
			predicate.and(qExceptionRequest.customerName
					.like("%" + exceptionRequestDTO.getCustomerName() + "%"));
		}
		// find by allowed amount
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getAllowedAmount())) {
			predicate.and(
					qExceptionRequest.allowedAmount.eq(exceptionRequestDTO.getAllowedAmount()));
		}
		// find by maker name
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getMakerName())) {
			predicate.and(qExceptionRequest.MakerName
					.like("%" + exceptionRequestDTO.getMakerName() + "%"));
		}
		// find by maker username
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getMakerUsername())) {
			predicate.and(qExceptionRequest.makerUsername
					.like("%" + exceptionRequestDTO.getMakerUsername() + "%"));
		}
		// find by requested amount
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getRequestedAmount())) {
			predicate.and(
					qExceptionRequest.requestedAmount.eq(exceptionRequestDTO.getRequestedAmount()));
		}
		// find by owner username
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getOwnerUsername())) {
			predicate.and(qExceptionRequest.ownerUsername
					.like("%" + exceptionRequestDTO.getOwnerUsername() + "%"));
		}
		// find by date insertion
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getDateInsertion())) {
			Timestamp StartTimesTamp =
					DateUtil.dateToDateTime(exceptionRequestDTO.getDateInsertion(), "00:00:01");
			Timestamp EndTimesTamp =
					DateUtil.dateToDateTime(exceptionRequestDTO.getDateInsertion(), "23:59:59");
			predicate.and(qExceptionRequest.dateInsertion.between(StartTimesTamp, EndTimesTamp));
		}
		// find by date update
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getDateLastUpdate())) {
			Timestamp StartTimesTamp =
					DateUtil.dateToDateTime(exceptionRequestDTO.getDateLastUpdate(), "00:00:01");
			Timestamp EndTimesTamp =
					DateUtil.dateToDateTime(exceptionRequestDTO.getDateLastUpdate(), "23:59:59");
			predicate.and(qExceptionRequest.dateLastUpdate.between(StartTimesTamp, EndTimesTamp));
		}
		// find by reject note description
		if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTO.getRejectNote())) {
			predicate.and(qExceptionRequest.rejectNote
					.like("%" + exceptionRequestDTO.getRejectNote() + "%"));
		}
		// find exception request by Access Branches for connected user
		else if (connectedUser.getCategory() != null
				&& connectedUser.getCategory().equals(UserCategory.MANAGMENT.name())
				&& !ACMValidationUtils.isNullOrEmpty(connectedUser.getAccessBranches())) {

			int[] arrayBranchIds = Arrays.asList(connectedUser.getAccessBranches().split(","))
					.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
			List<Integer> listBranchIds = new ArrayList<>(arrayBranchIds.length);
			for (int i : arrayBranchIds) {
				listBranchIds.add(Integer.valueOf(i));
			}
			predicate.and(qExceptionRequest.branchId.in(listBranchIds));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ExceptionRequestService#count(java.lang.String)
	 */
	@Override
	public ExceptionRequestCountDTO count() {

		ExceptionRequestCountDTO exceptionRequestCountDTO = new ExceptionRequestCountDTO();
		logger.info("START calculate COUNT For EXCEPTION REQUESTS TAB : {}");
		Long count = 0L;
		// INIT QLoan
		QExceptionRequest qExceptionRequest = QExceptionRequest.exceptionRequest;
		// INIT params
		ExceptionRequestDTO exceptionRequestDTO = new ExceptionRequestDTO();

		// execute query for Tab NEW
		exceptionRequestDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.NEW_STATUT_REQUEST).getKey());
		count = exceptionRequestRepository
				.count(buildQuery(exceptionRequestDTO, qExceptionRequest));
		exceptionRequestCountDTO.setCountNew(count);

		// execute query for Tab ACCEPTED
		exceptionRequestDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.ACCEPTED_STATUT_REQUEST).getKey());
		count = exceptionRequestRepository
				.count(buildQuery(exceptionRequestDTO, qExceptionRequest));
		exceptionRequestCountDTO.setCountAccepted(count);

		// execute query for Tab REJECTED
		exceptionRequestDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED_STATUT_REQUEST).getKey());
		count = exceptionRequestRepository
				.count(buildQuery(exceptionRequestDTO, qExceptionRequest));
		exceptionRequestCountDTO.setCountRejected(count);

		// execute query for Tab CANCELLED
		exceptionRequestDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED_STATUT_REQUEST).getKey());
		count = exceptionRequestRepository
				.count(buildQuery(exceptionRequestDTO, qExceptionRequest));
		exceptionRequestCountDTO.setCountCancelled(count);

		// execute query for Tab CLOSED
		exceptionRequestDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CLOSED_STATUT_REQUEST).getKey());
		count = exceptionRequestRepository
				.count(buildQuery(exceptionRequestDTO, qExceptionRequest));
		exceptionRequestCountDTO.setCountClosed(count);
		logger.info("Returning COUNT = {} For  EXCEPTION REQUEST TAB : {}", count);
		return exceptionRequestCountDTO;
	}
}
