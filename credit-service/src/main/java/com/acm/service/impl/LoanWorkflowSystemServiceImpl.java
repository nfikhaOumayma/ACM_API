/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistoryLoan;
import com.acm.client.ParametrageClient;
import com.acm.client.ReportingClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.service.AcmDocumentsService;
import com.acm.service.CustomerDecisionService;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.service.LoanDetailsService;
import com.acm.service.LoanParticipantsService;
import com.acm.service.LoanService;
import com.acm.service.LoanWorkflowSystemService;
import com.acm.service.NotificationsServices;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.Loan;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;

/**
 * {@link LoanWorkflowSystemServiceImpl} class The implementation of the service
 * {@link LoanWorkflowSystemService}.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@Service("loanWorkflowSystemServiceImpl")
public class LoanWorkflowSystemServiceImpl implements LoanWorkflowSystemService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanWorkflowSystemServiceImpl.class);

	/** The acm documents service. */
	@Autowired
	private AcmDocumentsService acmDocumentsService;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The loan details service. */
	@Autowired
	private LoanDetailsService loanDetailsService;

	/** The LoanApprovalHistorique service. */
	@Autowired
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The loanService. */
	@Autowired
	private LoanService loanService;

	/** The customer decision service. */
	@Autowired
	private CustomerDecisionService customerDecisionService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The userClient. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processDetectNewLoan(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@ProcessHistoryLoan(action = CommonAOPConstants.PROCESS_DETECT_NEW_LOAN)
	@Override
	public LoanDTO processDetectNewLoan(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin process Detect New Loan...");
		// logger.debug("{}", delegateExecution);
		// delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0).getIhmRoute());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		// update loan data
		return loanService.save(loanDTO.getLoanId(), loanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processCheckEtatRequiredDocs(org.activiti.engine.
	 * delegate.DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processCheckEtatRequiredDocs(LoanDTO loanDTO) throws ResourcesNotFoundException,
			UploadDocumentNotFoundException, CheckApprovelLevelException, ApiAbacusException,
			IOException, CreditException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		logger.info("Begin process Check Etat Required Docs...");
		// logger.debug("{}", delegateExecution);
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
			// process workflow for loan group
			actionUploadDocumentGroupe(loanDTO);
		}
		else {
			// check Required Document
			Boolean checkRequiredDocument = acmDocumentsService.checkRequiredDocument(loanDTO);
			if (Boolean.TRUE.equals(checkRequiredDocument)) {
				// update statutWorkflow
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getValue());
				// update EtapeWorkflow
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());
				// update next action
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_YES);

				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_SCREENING)).get(0)
						.getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
			}
			else {
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_NO);
				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
			}
			// update loan data
			loanService.save(loanDTO.getLoanId(), loanDTO);
		}
		return loanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowSystemService#processSubmitLoanApproval(org.activiti.engine.
	 * delegate.DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.PROCESS_SUBMIT_LOAN_APPROVAL)
	public LoanDTO processSubmitLoanApproval(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException {

		logger.info("Begin process submit Loan Approval...");
		// logger.debug("{}", delegateExecution);
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		List<SettingLevelProcessDTO> settingLevelProcessDTOs;
		// action Check Approbation Level Groupe
		if (loanDTO.getParentId() != 0) {
			// find group parent by ID
			LoanDTO groupLoanDTO = loanService.find(loanDTO.getParentId());
			settingLevelProcessDTOs = parametrageClient.find(new SettingLevelProcessDTO(
					Long.valueOf(loanDTO.getProductId()), groupLoanDTO.getApprovelAmount()));
		}
		else {
			// action Check Approbation Level
			settingLevelProcessDTOs = parametrageClient.find(new SettingLevelProcessDTO(
					Long.valueOf(loanDTO.getProductId()), loanDTO.getApprovelAmount()));
		}
		// init setting Level Process
		Integer settinglevel = settingLevelProcessDTOs.size();
		if (settinglevel != 0 && !loanDTO.getWorkflowNextAction()
				.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
			// enable critical data for customer and guarantors

			try {
				// enable critical data for Customer
				if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO())
						&& !ACMValidationUtils.isNullOrEmpty(loanDTO.getCustomerDTO().getId())) {
					loanDTO.getCustomerDTO().setEnableCriticalData(Boolean.TRUE);
					customerService.save(loanDTO.getCustomerDTO().getId(),
							loanDTO.getCustomerDTO());
					logger.info("Critical Data for customer = [{}] is enabled ",
							loanDTO.getCustomerDTO().getId());
				}

				// enable critical data for guarantors
				CustomerLinksRelationshipDTO paramGuarantors = new CustomerLinksRelationshipDTO();
				paramGuarantors.setIdLoan(loanDTO.getLoanId());
				paramGuarantors.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
				List<CustomerLinksRelationshipDTO> listLoanGuarantors =
						customerLinksRelationshipService.find(paramGuarantors);
				for (CustomerLinksRelationshipDTO guarantor : listLoanGuarantors) {
					if (!ACMValidationUtils.isNullOrEmpty(guarantor.getMember())
							&& !ACMValidationUtils.isNullOrEmpty(guarantor.getMember().getId())) {
						guarantor.getMember().setEnableCriticalData(Boolean.TRUE);
						customerService.save(guarantor.getMember().getId(), guarantor.getMember());
						logger.info("Critical Data for guarantor = [{}] is enabled ",
								guarantor.getMember().getId());
					}

				}
			}
			catch (CalculateAgeException | ResourcesNotFoundException e) {
				logger.error("Exception while updating critical data of customer");
				throw new EnableCriticalDataException(
						new ExceptionResponseMessage(CommonErrorCode.ENABLE_CRITICAL_DATA,
								CommonExceptionsMessage.ENABLE_CRITICAL_DATA),
						CommonExceptionsMessage.ENABLE_CRITICAL_DATA);
			}
			Integer currentEtape = loanDTO.getEtapeWorkflow();
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_NEXT_APPROVE_LEVEL);
			loanDTO.setStatutWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey());
			loanDTO.setStatutLibelle(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getValue());
			loanDTO.setEtapeWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey());
			// update status tab
			loanDTO.setStatut(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_PENDING_APPROVAL)
					.getKey());
			loanDTO.setIhmRoot(
					parametrageClient.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME))
							.get(0).getIhmRoute());

			// setting date change status
			loanDTO.setChangeDateStatusWorkflow(new Date());
			loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
			if (currentEtape == CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT)
					.getKey()
					|| currentEtape == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey()) {

				// update list participants
				loanParticipantsService
						.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

				// find default owner
				UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
				loanDTO.setOwner(foundedUserDTO.getResponsableId());
				loanDTO.setOwnerName(
						userClient.findByLogin(foundedUserDTO.getResponsableId()).getSimpleName());

				// setting list participants
				LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
						.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
				logger.info("(AUDIT / RISK) setting Loan Participants with ID = [{}] :: DONE",
						newLoanParticipantsDTO.getId());
			}
			else {
				// assign to default responsable
				UserDTO userConnect = getUsernameByPortfolioId(loanDTO);
				if (!userConnect.getResponsableId().equals("0")) {
					// update list participants
					LoanParticipantsDTO loanParticipantsDTO =
							new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner());
					loanParticipantsService.update(loanParticipantsDTO);
					// setting owner to responsable
					loanDTO.setOwner(userConnect.getResponsableId());
					loanDTO.setOwnerName(
							userClient.findByLogin(userConnect.getResponsableId()).getSimpleName());
					// setting list participants
					LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
							.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
					logger.info(
							"(processSubmitLoanApproval) setting Loan Participants with ID = [{}] :: DONE",
							newLoanParticipantsDTO.getId());
				}
			}
		}
		else {
			loanDTO.setWorkflowNextAction(
					ACMConstantWorkflowStatuts.ACTION_MISSING_APPROVEL_PROCESS_CONFIG);
			// action in workflow schema
			loanDTO.setEtapeWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
			// update status loan
			loanDTO.setStatutWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
			// update status tab
			loanDTO.setStatut(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REVIEW).getKey());
			loanDTO.setIhmRoot(
					parametrageClient.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME))
							.get(0).getIhmRoute());

			// setting date change status
			loanDTO.setChangeDateStatusWorkflow(new Date());
			loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

			// find default owner
			UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
			loanDTO.setOwner(foundedUserDTO.getLogin());
			loanDTO.setOwnerName(foundedUserDTO.getSimpleName());
		}
		logger.info("End process submit Loan Approval...");
		// update loan data
		return loanService.save(loanDTO.getLoanId(), loanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processEtatValide(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.PROCESS_ETAT_VALIDE)
	public LoanDTO processEtatValide(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin process Etat Valide (Approve on CBS)...");
		// logger.debug("{}", delegateExecution);
		//
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		// update statutWorkflow
		loanDTO.setStatutWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		loanDTO.setStatutLibelle(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getValue());
		// update EtapeWorkflow
		loanDTO.setEtapeWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.DISBURSEMENT_CASE_CLOSURE).getKey());
		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED).getKey());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update loan data
		loanService.save(loanDTO.getLoanId(), loanDTO);

		logger.info("Process (Approve on CBS) :: DONE");
		return loanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processEtatRejete(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processEtatRejete(LoanDTO loanDTO)
			throws ResourcesNotFoundException, IOException, ApiAbacusException {

		logger.info("Begin process Etat Rejete...");
		// logger.debug("{}", delegateExecution);
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0).getIhmRoute());
		// The application is rejected in the CBS with the reason via API (Cancelled API)
		try {
			transversClient.cancelLoan(loanDTO);
		}
		catch (Exception e) {
			logger.error("Failed to processEtatRejete loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e != null && e.getMessage() != null && e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
		return loanService.rejected(loanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processEtatCanceled(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processEtatCanceled(LoanDTO loanDTO)
			throws ResourcesNotFoundException, ApiAbacusException, IOException {

		logger.info("Begin process Etat Canceled...");
		// logger.debug("{}", delegateExecution);
		//
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		// update status loan
		loanDTO.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getKey());
		loanDTO.setStatutLibelle(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.CANCELLED).getValue());

		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_CANCELLED).getKey());
		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0).getIhmRoute());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		try {
			transversClient.cancelLoan(loanDTO);
		}
		catch (Exception e) {
			logger.error("Failed to cancel loan {}", e.getMessage());
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
			// INIT error message
			String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
			if (e != null && e.getMessage() != null && e.getMessage().contains("errorMessage")) {
				String msgFromTransversApi = e.getMessage().substring(e.getMessage().indexOf('{'));
				final JsonNode jsonNode = new ObjectMapper().readTree(msgFromTransversApi);
				messageError = jsonNode.get("errorMessage").asText();
			}
			// Fire Exception
			throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
		}
		// The application is rejected in the CBS with the reason via API (Cancelled API)
		return loanService.declined(loanDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processCheckApprobationLevel(org.activiti.engine.
	 * delegate.DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processCheckApprobationLevel(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckApprovelLevelException {

		logger.info("Begin process Check Approbation Level...");
		// logger.debug("{}", delegateExecution);
		//
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);
		List<SettingLevelProcessDTO> settingLevelProcessDTOs;
		// action Check Approbation Level Groupe
		if (loanDTO.getParentId() != 0) {
			settingLevelProcessDTOs = parametrageClient.find(new SettingLevelProcessDTO(
					Long.valueOf(loanDTO.getProductId()), loanDTO.getApprovelAmountGroupe()));
		}
		else {
			// action Check Approbation Level
			settingLevelProcessDTOs = parametrageClient.find(new SettingLevelProcessDTO(
					Long.valueOf(loanDTO.getProductId()), loanDTO.getApprovelAmount()));
		}
		// init setting Level Process
		Integer settinglevel = settingLevelProcessDTOs.size();
		if (settinglevel != 0) {
			// init Notification description message
			String actionDescription = "Loan has been submited to be approved at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ loanDTO.getOwnerName();
			if ((settinglevel == 1 && loanDTO.getStatutWorkflow() == CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey())
					|| (settinglevel == 2 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey())
					|| (settinglevel == 3 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getKey())
					|| (settinglevel == 4 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getKey())) {

				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_APPROVED);
				processAssignTo(loanDTO);
				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
			}
			else if ((settinglevel == 2 && loanDTO.getStatutWorkflow() == CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey())
					|| (settinglevel == 3 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey())
					|| (settinglevel == 4 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey())) {

				loanDTO = nextApproveAction(loanDTO, ACMConstantWorkflowStatuts.APPROVAL_L2);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// Send Notif Approve L2/L3/L4
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Audit Notification to L2 with ID = [{}] has been inserted.",
						notificationsDTO);
			}
			else if ((settinglevel == 3 && loanDTO.getStatutWorkflow() == CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey())
					|| (settinglevel == 4 && loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey())) {

				loanDTO = nextApproveAction(loanDTO, ACMConstantWorkflowStatuts.APPROVAL_L3);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// Send Notif Approve L2/L3/L4
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Audit Notification  to L3 with ID = [{}] has been inserted.",
						notificationsDTO);
			}
			else if (settinglevel == 4 && loanDTO.getStatutWorkflow() == CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getKey()) {

				loanDTO = nextApproveAction(loanDTO, ACMConstantWorkflowStatuts.APPROVAL_L4);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// Send Notif Approve L2/L3/L4
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Audit Notification  to L4 with ID = [{}] has been inserted.",
						notificationsDTO);
			}
		}
		else {
			logger.error("Failed to find Check Approvel Level !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CheckApprovelLevelException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment
							.getProperty("workflow.exception.message.not.found.checkApprovelLevel")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		return loanDTO;
	}

	/**
	 * Next approve action.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param nextStep the next step
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private LoanDTO nextApproveAction(LoanDTO loanDTO, String nextStep)
			throws ResourcesNotFoundException {

		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_NEXT_APPROVE_LEVEL);
		loanDTO.setStatutWorkflow(CommonFunctions.mappingStatus(nextStep).getKey());
		loanDTO.setStatutLibelle(CommonFunctions.mappingStatus(nextStep).getValue());
		loanDTO.setEtapeWorkflow(CommonFunctions.mappingStatus(nextStep).getKey());

		// update list participants
		loanParticipantsService
				.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

		// assign to default responsable.
		UserDTO userConnect = CommonFunctions.getConnectedUser(logger);
		if (!userConnect.getResponsableId().equals("0")) {
			loanDTO.setOwner(userConnect.getResponsableId());
			loanDTO.setOwnerName(
					userClient.findByLogin(userConnect.getResponsableId()).getSimpleName());
		}

		// setting list participants
		LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
				.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
		logger.info("(nextApproveAction) setting Loan Participants with ID = [{}] :: DONE",
				newLoanParticipantsDTO.getId());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		return loanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processCheckGuarantorCollateral(org.activiti.engine
	 * .delegate.DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processCheckGuarantorCollateral(LoanDTO loanDTO)
			throws ResourcesNotFoundException {

		logger.info("Begin process Check Guarantor / Collateral...");
		// logger.debug("{}", delegateExecution);
		//
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		// check Guarantor/Collateral configuration
		switch (loadConfigGuarantorCollateral(loanDTO)) {
			// ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY
			case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY:
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY);
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getValue());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_CHECK_GUARANTOR))
						.get(0).getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			// ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY
			case ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY:

				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY);
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getValue());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_CHECK_COLLATERAL))
						.get(0).getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			// ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL
			case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL:

				loanDTO.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL);
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_CHECK_GUARANTOR))
						.get(0).getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			// ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL
			case ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL:
				// update status loan to ADD_DOCUMENTS
				loanDTO.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL);
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_UPLOAD_DOCUMENT))
						.get(0).getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			default:
				break;
		}
		// returning loan data
		return loanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processDataControlCBS(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processDataControlCBS(LoanDTO loanDTO) throws ResourcesNotFoundException {

		// delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);
		// logger.info("Begin process data Control CBS...");
		// logger.debug("{}", delegateExecution);
		if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
			// process data Control CBS for given group
			processDataControlCBSGroupe(loanDTO);
		}
		else {
			// init instant datetime
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
			String currentDate = dateFormat.format(new Date());
			String note = "";
			// ### check Guarantor/Collateral configuration ###
			List<String> missingData = new ArrayList<>();

			Boolean checkGuarantorCollateralABACUS = Boolean.FALSE;
			Boolean checkGuarantorACM = Boolean.FALSE;
			if (loanDTO.getLoanApplicationStatus().equals(CommonConstants.TOPUP)
					|| loanDTO.getLoanApplicationStatus().equals(CommonConstants.REFINANCE)) {
				checkGuarantorCollateralABACUS = Boolean.TRUE;
				checkGuarantorACM = Boolean.TRUE;
			}
			else {
				switch (loadConfigGuarantorCollateral(loanDTO)) {
					case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY:
						// Load grantor list from abacus DB
						List<GuarantorDTO> guarantorOnly =
								loanDetailsService.findGuarantors(loanDTO.getIdLoanExtern());
						// List guarantor from ACM
						CustomerLinksRelationshipDTO paramsGuarantorOnly =
								new CustomerLinksRelationshipDTO();
						paramsGuarantorOnly.setIdLoan(loanDTO.getLoanId());
						paramsGuarantorOnly.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						List<CustomerLinksRelationshipDTO> listGuarantorOnly =
								customerLinksRelationshipService.find(paramsGuarantorOnly);

						if (!ACMValidationUtils.isNullOrEmpty(guarantorOnly)
								&& !ACMValidationUtils.isNullOrEmpty(listGuarantorOnly)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Guarantor in the loan application. Please review.";
							missingData.add(CommonErrorCode.GUARANTOR_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY:
						// load && check list collateral from abacus DB
						List<CollaterolDTO> collaterolOnly =
								loanDetailsService.findCollaterols(loanDTO.getIdLoanExtern());
						if (!ACMValidationUtils.isNullOrEmpty(collaterolOnly)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Collaterals in the loan application. Please review.";
							missingData.add(CommonErrorCode.COLLATEROL_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL:
						// load guarantor list from abacus DB
						List<GuarantorDTO> guarantorDTOs =
								loanDetailsService.findGuarantors(loanDTO.getIdLoanExtern());

						// List guarantor from ACM
						CustomerLinksRelationshipDTO paramsGuarantorCollateral =
								new CustomerLinksRelationshipDTO();
						paramsGuarantorCollateral.setIdLoan(loanDTO.getLoanId());
						paramsGuarantorCollateral
								.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						List<CustomerLinksRelationshipDTO> listGuarantorCollateral =
								customerLinksRelationshipService.find(paramsGuarantorCollateral);

						// load && check list collateral from abacus DB
						List<CollaterolDTO> collaterolDTOs =
								loanDetailsService.findCollaterols(loanDTO.getIdLoanExtern());
						if (!ACMValidationUtils.isNullOrEmpty(guarantorDTOs)
								&& !ACMValidationUtils.isNullOrEmpty(listGuarantorCollateral)
								&& !ACMValidationUtils.isNullOrEmpty(collaterolDTOs)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Guarantor or Collateral in the loan application. Please review.";
							missingData
									.add(CommonErrorCode.GUARANTOR_COLLATEROL_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL:
						checkGuarantorCollateralABACUS = Boolean.TRUE;
						checkGuarantorACM = Boolean.TRUE;
						break;

					default:
						break;
				}
			}
			logger.info("checkGuarantorCollateral in ABACUS DB = {}",
					checkGuarantorCollateralABACUS);
			logger.info("checkGuarantorCollateral in ACM DB = {}", checkGuarantorACM);

			// ### check Document ###
			Boolean checkDocument = acmDocumentsService.checkRequiredDocument(loanDTO);
			if (Boolean.FALSE.equals(checkDocument)) {
				String noteRequiredDocument =
						"Problem with data verification: Missing Required Documents in the loan application. Please review.";
				// setting note
				note = ACMValidationUtils.isNullOrEmpty(note) ? noteRequiredDocument
						: note + " | " + noteRequiredDocument;
				missingData.add(CommonErrorCode.UPLOAD_DOCUMENT_CODE_DATA_NOT_FOUND);
			}

			// ### check information loan ###
			Boolean checkInformationLoan = Boolean.FALSE;
			// find data of the current loan in ACM DB
			LoanDTO currentLoanACM = loanService.find(loanDTO.getLoanId());
			// find data of the current loan in ABACUS DB
			LoanDTO currentLoanABACUS = transversClient.findLoan(currentLoanACM.getIdLoanExtern());
			// compare LoanDTO objects
			Map<Boolean, String> doCompareLoanMap =
					doCompareLoan(currentLoanACM, currentLoanABACUS);

			// get key from map
			Boolean key = doCompareLoanMap.entrySet().stream().findFirst().get().getKey();
			// processing data
			if (Boolean.TRUE.equals(key)) {
				checkInformationLoan = Boolean.TRUE;
			}
			else {
				// get message from map
				String message = doCompareLoanMap.entrySet().stream().findFirst().get().getValue();
				String noteCheckInformationLoan =
						"Problem with data verification, There are some differences in loan informations between the Core Banking and ACM : "
								+ message;
				note = ACMValidationUtils.isNullOrEmpty(note) ? noteCheckInformationLoan
						: note + " | " + noteCheckInformationLoan;
				missingData.add(CommonErrorCode.LOAN_INFORMATION_CHANGE);
			}

			// build Note
			String connectedUserName = CommonFunctions.getConnectedUser(logger).getFullName();
			String noteHistorique = String.format(
					"Date: %s | Added By: %s | Action: Data Verification | Note: %s", currentDate,
					loanDTO.getOwnerName() != null ? loanDTO.getOwnerName() : connectedUserName,
					note);
			logger.info("NoteHistorique = {}", noteHistorique);

			// check if all TRUE ==> pass to next step in process
			if (Boolean.TRUE.equals(checkDocument) && Boolean.TRUE.equals(checkInformationLoan)
					&& Boolean.TRUE.equals(checkGuarantorCollateralABACUS)
					&& Boolean.TRUE.equals(checkGuarantorACM)) {

				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_YES);
				if (loanDTO.getEtapeWorkflow() <= 23) {
					// action in workflow schema
					loanDTO.setEtapeWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
							.getKey());
					loanDTO.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
							.getValue());
					// update status loan
					loanDTO.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
							.getKey());

					// update status tab
					loanDTO.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED)
							.getKey());
					loanDTO.setIhmRoot(parametrageClient
							.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
							.getIhmRoute());

					// update loan data : ASSIGN the loan to BRANCH_OPERATION GROUP
					setLoanToGroupOfUsers(CommonConstants.BRANCH_OPERATION, loanDTO);
				}
				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// setting note
				loanDTO.setNote(noteHistorique);

				// update loan data
				LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);

				// Save customerDecision history
				CustomerDecisionDTO customerDecisionDTO =
						new CustomerDecisionDTO(
								DateUtil.setCurrentTimeToDate(
										loanDTO.getContactDateCustomerDecision() != null
												? loanDTO.getContactDateCustomerDecision()
												: new Date()),
								loanDTO.getCommentsCustomerDecision(), loanDTO.getLoanId(),
								CommonFunctions.mappingStatus(
										ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_AGREED)
										.getKey(),
								loanDTO.getApprovelAmount());
				CustomerDecisionDTO newCustomerDecisionDTO =
						customerDecisionService.save(customerDecisionDTO);
				logger.info(
						"Customer Decision with status : {} was successfully inserted for loan : {}",
						newCustomerDecisionDTO.getStatusLibelle(), loanDTO.getAccountNumber());

				if (loanDTO.getEtapeWorkflow() <= 23) {
					String actionDescription = "-- CUSTOMER DECISION-- Step has been validated at"
							+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
							+ loanDTO.getOwnerName();
					// Send notification to BRANCH_OPERATION_GROUP users

					if (loanDTO.getAssignedToOneUser()) {
						notifyOneUserPerGroup(loanDTO, CommonConstants.BRANCH_OPERATION,
								actionDescription);
					}
					else {
						notifyUsersPerGroup(loanDTO, CommonConstants.BRANCH_OPERATION,
								actionDescription);
					}
				}
				if (newLoanDTO.getParentId() == 0) {
					// Send Mail
					sendMail(new MailLoanDTO(newLoanDTO,
							new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
									!ACMValidationUtils
											.isNullOrEmpty(newLoanDTO.getCustomerDTO().getEmail())
													? newLoanDTO.getCustomerDTO().getEmail()
													: defaultACMReceiverMail,
									"Your loan application has been approved : "
											+ newLoanDTO.getAccountNumber(),
									""),
							MailBuilderMethod.BUILD_CUSTOMER_ACCEPT, newLoanDTO.getUpdatedBy()));
				}
			}
			else {
				// redirect loan to review
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_NO);
				if (loanDTO.getEtapeWorkflow() <= 23) {
					// action in workflow schema
					loanDTO.setEtapeWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
					// update status loan
					loanDTO.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
					// update status tab
					loanDTO.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REVIEW).getKey());
				}
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
						.getIhmRoute());
				loanDTO.setListMissingData(missingData);

				// setting note
				loanDTO.setNote(noteHistorique);

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// save historique approvel data
				LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO =
						new LoanApprovalHistoriqueDTO(loanDTO,
								loanDTO.getApprovelAmount().longValue(),
								CommonFunctions.mappingStatus(
										ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
										.getKey(),
								noteHistorique, getLastAppovelLevel(loanDTO));
				LoanApprovalHistoriqueDTO newLoanApprovalHistoriqueDTO =
						loanApprovalHistoriqueService
								.saveAndSetApprovalLabel(loanApprovalHistoriqueDTO);
				logger.info(
						"Loan Approval Historique with status : {} was successfully inserted for loan : {} :: RAISON = [{}]",
						newLoanApprovalHistoriqueDTO.getApprovalDesicionLabel(),
						loanDTO.getAccountNumber(), noteHistorique);
			}
		}
		return loanDTO;
	}

	/**
	 * Do compare loanDTO object => fields selected to be compare are those defined in :
	 * LoanAbacusRowMapper.
	 *
	 * @author HaythemBenizid
	 * @param currentLoanACM the current loan ACM
	 * @param currentLoanABACUS the current loan ABACUS
	 * @return the map
	 */
	private Map<Boolean, String> doCompareLoan(LoanDTO currentLoanACM, LoanDTO currentLoanABACUS) {

		// init MAP resultat
		Map<Boolean, String> map = new HashMap<>();
		map.put(Boolean.TRUE, "");
		return map;
		/*// init message
		String message = "%s are not Equal between the CoreBanking and ACM for LOAN "
				+ currentLoanACM.getAccountNumber();
		List<String> errorMessages = new ArrayList<>();
		Boolean error = Boolean.FALSE;

		// PRODUCT
		logger.info("currentLoanACM.getProductId = {} <==> currentLoanABACUS.getProductId = {} ",
				currentLoanACM.getProductId(), currentLoanABACUS.getProductId());
		if (currentLoanACM.getProductId().compareTo(currentLoanABACUS.getProductId()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "PRODUCT"));
		}

		// PRODUCT_RATE
		logger.info(
				"currentLoanACM.getProductRate = {} <==> currentLoanABACUS.getProductRate = {} ",
				currentLoanACM.getProductRate(), currentLoanABACUS.getProductRate());
		// ROUND UP APR
		currentLoanACM.setProductRate(NumberUtils.roundBigDecimal(currentLoanACM.getProductRate(),
				2, BigDecimal.ROUND_HALF_UP));
		currentLoanABACUS.setProductRate(NumberUtils
				.roundBigDecimal(currentLoanABACUS.getProductRate(), 2, BigDecimal.ROUND_HALF_UP));
		logger.info(
				"After BigDecimal.ROUND_HALF_UP : currentLoanACM.getProductRate = {} <==> currentLoanABACUS.getProductRate = {} ",
				currentLoanACM.getProductRate(), currentLoanABACUS.getProductRate());
		if (currentLoanACM.getProductRate().compareTo(currentLoanABACUS.getProductRate()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "PRODUCT_RATE"));
		}

		// APPLY_AMOUNT_TOTAL
		logger.info(
				"currentLoanACM.getApplyAmountTotal = {} <==> currentLoanABACUS.getApplyAmountTotal = {} ",
				currentLoanACM.getApplyAmountTotal(), currentLoanABACUS.getApplyAmountTotal());
		if (currentLoanACM.getApplyAmountTotal()
				.compareTo(currentLoanABACUS.getApplyAmountTotal()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "APPLY_AMOUNT_TOTAL"));
		}

		// ISSUE_FEE_AMOUNT
		logger.info(
				"currentLoanACM.getIssueFeeAmount = {} <==> currentLoanABACUS.getIssueFeeAmount = {} ",
				currentLoanACM.getIssueFeeAmount(), currentLoanABACUS.getIssueFeeAmount());
		if (currentLoanACM.getIssueFeeAmount()
				.compareTo(currentLoanABACUS.getIssueFeeAmount()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "ISSUE_FEE_AMOUNT"));
		}

		// ISSUE_DATE
		logger.info("currentLoanACM.getIssueDate = {} <==> currentLoanABACUS.getIssueDate = {} ",
				currentLoanACM.getIssueDate(), currentLoanABACUS.getIssueDate());
		if (!DateUtil.formatDate(currentLoanACM.getIssueDate(), CommonConstants.PATTREN_DATE)
				.equals(DateUtil.formatDate(currentLoanABACUS.getIssueDate(),
						CommonConstants.PATTREN_DATE))) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "ISSUE_DATE"));
		}

		// INITIAL_PAYMENT_DATE
		logger.info(
				"currentLoanACM.getInitialPaymentDate = {} <==> currentLoanABACUS.getInitialPaymentDate = {} ",
				currentLoanACM.getInitialPaymentDate(), currentLoanABACUS.getInitialPaymentDate());
		if (!DateUtil
				.formatDate(currentLoanACM.getInitialPaymentDate(), CommonConstants.PATTREN_DATE)
				.equals(DateUtil.formatDate(currentLoanABACUS.getInitialPaymentDate(),
						CommonConstants.PATTREN_DATE))) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "INITIAL_PAYMENT_DATE"));
		}

		// APPROVE_AMOUNT_TOTAL
		logger.info(
				"currentLoanACM.getApprovelAmount = {} <==> currentLoanABACUS.getApplyAmountTotal = {} ",
				currentLoanACM.getApprovelAmount(), currentLoanABACUS.getApplyAmountTotal());
		if (currentLoanACM.getApprovelAmount()
				.compareTo(currentLoanABACUS.getApplyAmountTotal()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "APPROVE_AMOUNT_TOTAL"));
		}

		// APR
		logger.info("currentLoanACM.getApr = {} <==> currentLoanABACUS.getApr = {} ",
				currentLoanACM.getApr(), currentLoanABACUS.getApr());
		// ROUND UP APR
		currentLoanACM.setApr(
				NumberUtils.roundBigDecimal(currentLoanACM.getApr(), 2, BigDecimal.ROUND_HALF_UP));
		currentLoanABACUS.setApr(NumberUtils.roundBigDecimal(currentLoanABACUS.getApr(), 2,
				BigDecimal.ROUND_HALF_UP));
		logger.info(
				"After BigDecimal.ROUND_HALF_UP : currentLoanACM.getApr = {} <==> currentLoanABACUS.getApr = {} ",
				currentLoanACM.getApr(), currentLoanABACUS.getApr());
		if ((currentLoanACM.getApr()).compareTo(currentLoanABACUS.getApr()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "APR"));
		}

		// EFFECTIVE_INT_RATE
		logger.info(
				"currentLoanACM.getEffectiveIntRate = {} <==> currentLoanABACUS.getEffectiveIntRate = {} ",
				currentLoanACM.getEffectiveIntRate(), currentLoanABACUS.getEffectiveIntRate());
		// ROUND UP EffectiveIntRate
		currentLoanACM.setEffectiveIntRate(NumberUtils.roundBigDecimal(
				currentLoanACM.getEffectiveIntRate(), 2, BigDecimal.ROUND_HALF_UP));
		currentLoanABACUS.setEffectiveIntRate(NumberUtils.roundBigDecimal(
				currentLoanABACUS.getEffectiveIntRate(), 2, BigDecimal.ROUND_HALF_UP));
		logger.info(
				"After BigDecimal.ROUND_HALF_UP : currentLoanACM.getEffectiveIntRate = {} <==> currentLoanABACUS.getEffectiveIntRate = {} ",
				currentLoanACM.getEffectiveIntRate(), currentLoanABACUS.getEffectiveIntRate());
		// compare EffectiveIntRate
		if (currentLoanACM.getEffectiveIntRate()
				.compareTo(currentLoanABACUS.getEffectiveIntRate()) != 0) {
			error = Boolean.TRUE;
			errorMessages.add(String.format(message, "EFFECTIVE_INT_RATE"));
		}

		// processing data after check
		if (Boolean.TRUE.equals(error)) {
			map.put(Boolean.FALSE, String.join(" | ", errorMessages));
		}
		else {
			map.put(Boolean.TRUE, "");
		}
		// returning data
		logger.info("Result check : {}", map);
		return map;
		*/
	}

	/**
	 * get Last Approvel Level.
	 * 
	 * @author moezMhiri
	 * @param loanDTO the loan DTO
	 * @return Integer
	 */
	private Integer getLastAppovelLevel(LoanDTO loanDTO) {

		Integer lastApprovelLevel = 0;
		// action Check Approbation Level
		List<SettingLevelProcessDTO> settingLevelProcessDTOs =
				parametrageClient.find(new SettingLevelProcessDTO(
						Long.valueOf(loanDTO.getProductId()), loanDTO.getApprovelAmount()));
		// init setting Level Process
		Integer settinglevel = settingLevelProcessDTOs.size();
		switch (settinglevel) {
			case 1:
				lastApprovelLevel = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey();
				break;
			case 2:
				lastApprovelLevel = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L2).getKey();
				break;
			case 3:
				lastApprovelLevel = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L3).getKey();
				break;
			case 4:
				lastApprovelLevel = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getKey();
				break;

			default:
				break;
		}
		return lastApprovelLevel;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowSystemService#processAssignTo(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processAssignTo(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin process Assign To...");
		// logger.debug("{}", delegateExecution);
		//
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// delegateExecution.setVariable(CommonConstants.TIMER_TASK_REMAINDER_KEY,
		// CommonConstants.TIMER_TASK_REMAINDER_VALUE);

		// update list participants
		loanParticipantsService
				.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

		// assign to default ACCOUNTPORTFOLIO_CODE
		UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
		loanDTO.setOwner(foundedUserDTO.getLogin());
		loanDTO.setOwnerName(foundedUserDTO.getSimpleName());
		// setting list participants
		LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
				.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
		logger.info("(processAssignTo) setting Loan Participants with ID = [{}] :: DONE",
				newLoanParticipantsDTO.getId());

		// update status loan
		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_APPROVED);
		loanDTO.setStatutWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION).getKey());
		loanDTO.setStatutLibelle(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION).getValue());
		loanDTO.setEtapeWorkflow(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.CUSTOMER_DECISION).getKey());
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED).getKey());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// create notifcation only for loan INDIV / GROUPE (parent loan) / ORG
		if (loanDTO.getParentId() == 0) {
			// Notification
			String actionDescription = "Loan has been approved at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ CommonFunctions.getConnectedUser(logger).getFullName();
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO).getLogin(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_APPROVE,
							actionDescription, loanDTO, null));
			logger.info("New approved Notification [{}] has been inserted.", notificationsDTO);
		}
		// update loan data
		return loanService.save(loanDTO.getLoanId(), loanDTO);
	}

	/**
	 * Load config guarantor collateral.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the string
	 */
	private String loadConfigGuarantorCollateral(LoanDTO loanDTO) {

		Preconditions.checkNotNull(loanDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		String resultatCheck = ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL;
		// find config by product ID & mandatory column is TRUE
		List<SettingGurantorCollateralDTO> settingGurantorCollateralDTOs = parametrageClient
				.find(new SettingGurantorCollateralDTO(loanDTO.getProductId(), Boolean.TRUE));
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTOs)) {
			// init list code
			List<String> settings = new ArrayList<>();
			settingGurantorCollateralDTOs.stream().forEach(
					settingGurantorCollateral -> settings.add(settingGurantorCollateral.getCode()));
			// init list code process stored for given loan from Loan_instance
			List<Integer> codeProcess = new ArrayList<>();
			loanDTO.getLoanInstancesDtos().stream()
					.forEach(loanInstance -> codeProcess.add(loanInstance.getCode()));
			// check && return next step
			if (settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR)
					&& settings
							.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL)
					&& codeProcess.contains(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey())
					&& codeProcess.contains(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getKey())) {
				resultatCheck = ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL;
			}
			else if (settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR)
					&& codeProcess.contains(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.GUARANTOR).getKey())) {
				resultatCheck = ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY;
			}
			else if (settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL)
					&& codeProcess.contains(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getKey())) {
				resultatCheck = ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY;
			}
		}
		logger.info("LOAD Config Guarantor Collateral. resultatCheck= {} ", resultatCheck);
		return resultatCheck;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processCheckScreening(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processCheckScreening(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Start processing processCheckScreening()");
		// delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);

		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_SCREENING_YES);
		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update loan data
		loanService.save(loanDTO.getLoanId(), loanDTO);
		logger.info("Processing processCheckScreening() :: DONE");
		LoanDTO newloanDTO = processCheckFieldVisit(loanDTO);
		return newloanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowSystemService#processCheckFieldVisit(org.activiti.engine.delegate
	 * .DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processCheckFieldVisit(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Start processing processCheckFieldVisit()");
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// find config by product ID & mandatory column is TRUE
		List<SettingRequiredStepDTO> settingRequiredStepDTOs = parametrageClient
				.find(new SettingRequiredStepDTO(loanDTO.getProductId(), Boolean.TRUE));

		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTOs)) {
			List<String> settingsCode = new ArrayList<>();
			// filter list code REQUIRED STEP
			settingRequiredStepDTOs.stream()
					.forEach(setting -> settingsCode.add(setting.getCode()));

			// INIT list code process stored for given loan from Loan_instance
			List<Integer> codeProcess = new ArrayList<>();
			loanDTO.getLoanInstancesDtos().stream()
					.forEach(loanInstance -> codeProcess.add(loanInstance.getCode()));

			// check "STEP_FIELD_VISIT" from list workflow process
			if (settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_FIELD_VISIT)
					&& codeProcess.contains(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey())) {
				// update status loan
				loanDTO.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.ACTION_REQUIRED_FIELD_VISIT);
				// update status loan
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getValue());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_FIELD_VISIT))
						.get(0).getIhmRoute());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
			}
			else {
				// No setting REQUIRED STEP config was founded
				loanDTO.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.ACTION_NOT_REQUIRED_FIELD_VISIT);
				LoanDTO newLoanDTO = processAuditRisk(loanDTO);
				return newLoanDTO;
			}
		}
		else {
			// No setting REQUIRED STEP config was founded
			loanDTO.setWorkflowNextAction(
					ACMConstantWorkflowStatuts.ACTION_NOT_REQUIRED_FIELD_VISIT);
			LoanDTO newLoanDTO = processAuditRisk(loanDTO);
			return newLoanDTO;
		}
		logger.info("Processing processCheckFieldVisit() :: DONE");
		return loanDTO;
	}

	/**
	 * Sets the loan to group of users.
	 * 
	 * @author idridi
	 * @param groupeCode the groupe code
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 */
	public LoanDTO setLoanToGroupOfUsers(String groupeCode, LoanDTO loanDTO) {

		// update loan data
		// find group by code and step workflow
		GroupeDTO groupeDTO = parametrageClient.findGroupeByCode(groupeCode);
		// get the list of users that have loanBranch in their accessBranch
		List<UserDTO> userDTOParam = userClient.getUsersWithLoanBranchInTheirAccessBranches(
				loanDTO.getBranchID(), groupeDTO.getUserDTOs());
		// if the group has only one user( with loan branch IN his list of access branch) then
		// assign to this user
		if (userDTOParam.size() == 1) {
			loanDTO.setOwner(userDTOParam.get(0).getLogin());
			loanDTO.setOwnerName(userDTOParam.get(0).getSimpleName());
			loanDTO.setAssignedToOneUser(Boolean.TRUE);
			// setting list participants
			LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
					.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
			logger.info("(process Audit-Risk ) setting Loan Participants with ID = [{}] :: DONE",
					newLoanParticipantsDTO.getId());
		}
		else {
			// set owner and owner name null
			loanDTO.setOwner(null);
			loanDTO.setOwnerName(null);
			// set owner group and owner group name
			loanDTO.setGroupOwner(groupeDTO.getCode());
			loanDTO.setGroupOwnerName(groupeDTO.getLibelle());
			loanDTO.setAssignedToOneUser(Boolean.FALSE);
		}
		return loanDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowSystemService#processAuditRisk(org.activiti.engine.delegate.
	 * DelegateExecution, com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO processAuditRisk(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Start processing processAuditRisk()");
		// // delegateExecution.setVariable(CommonConstants.CONST_LOAN_ID, loanDTO.getLoanId());
		// delegateExecution.setVariable(CommonConstants.PROCESS_OBJECT_LOAN, loanDTO);
		// find config by product ID & mandatory column is TRUE
		List<SettingRequiredStepDTO> settingRequiredStepDTOs = parametrageClient
				.find(new SettingRequiredStepDTO(loanDTO.getProductId(), Boolean.TRUE));

		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTOs)) {
			List<String> settingsCode = new ArrayList<>();
			// filter list code REQUIRED STEP
			settingRequiredStepDTOs.stream()
					.forEach(setting -> settingsCode.add(setting.getCode()));
			Boolean audit = Boolean.FALSE;
			Boolean risk = Boolean.FALSE;
			// check "STEP_AUDIT_REVIEW" from config workflow process
			if (settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_AUDIT_REVIEW)) {
				audit = Boolean.TRUE;
			}

			// check "STEP_RISK_REVIEW" from config workflow process
			if (settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_RISK_REVIEW)) {
				risk = Boolean.TRUE;
			}

			// check next IHM
			if (Boolean.TRUE.equals(audit) && Boolean.TRUE.equals(risk)) {
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_AUDIT_RISK);

				// update status loan
				loanDTO.setStatutWorkflow(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey());
				loanDTO.setStatutLibelle(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getValue());
				loanDTO.setEtapeWorkflow(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
						.getIhmRoute());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());

				setLoanToGroupOfUsers(CommonConstants.BRANCH_AUDITOR, loanDTO);

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// NOTIFICATION
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.BRANCH_AUDITOR, "");
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.BRANCH_AUDITOR, "");
				}
			}
			else if (Boolean.TRUE.equals(audit)) {
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_AUDIT_ONLY);

				// update status loan
				loanDTO.setStatutWorkflow(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey());
				loanDTO.setEtapeWorkflow(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey());
				loanDTO.setStatutLibelle(
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getValue());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
						.getIhmRoute());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());

				// assign to
				setLoanToGroupOfUsers(CommonConstants.BRANCH_AUDITOR, loanDTO);

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				// NOTIFICATION
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.BRANCH_AUDITOR, "");
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.BRANCH_AUDITOR, "");
				}
			}
			else if (Boolean.TRUE.equals(risk)) {
				// loading config "RISK_AMOUNT_VALIDATION"
				AcmEnvironnementDTO environnementDTO =
						parametrageClient.find("RISK_AMOUNT_VALIDATION");
				logger.info("{}", environnementDTO);
				if (!ACMValidationUtils.isNullOrEmpty(environnementDTO)) {
					// getting configured risk amount
					BigDecimal riskAmount = new BigDecimal(environnementDTO.getValue());
					// check next step based on loan amount
					if (loanDTO.getApplyAmountTotal().compareTo(riskAmount) > 0) {
						loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_RISK_ONLY);

						// update status loan
						loanDTO.setStatutWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey());
						loanDTO.setStatutLibelle(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getValue());
						loanDTO.setEtapeWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey());
						loanDTO.setIhmRoot(parametrageClient
								.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
								.getIhmRoute());
						// update status tab
						loanDTO.setStatut(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS)
								.getKey());

						// update list participants
						LoanParticipantsDTO loanParticipantsDTO =
								new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner());
						loanParticipantsService.update(loanParticipantsDTO);

						// assign to
						setLoanToGroupOfUsers(CommonConstants.RISK_MANAGER, loanDTO);
						// setting date change status
						loanDTO.setChangeDateStatusWorkflow(new Date());
						loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
						// update loan data
						loanService.save(loanDTO.getLoanId(), loanDTO);
						// NOTIFICATION
						String actionDescription = "-- AUDIT -- Step has been validated at "
								+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
								+ " By " + loanDTO.getOwnerName();
						if (loanDTO.getAssignedToOneUser()) {
							notifyOneUserPerGroup(loanDTO, CommonConstants.RISK_MANAGER,
									actionDescription);
						}
						else {
							notifyUsersPerGroup(loanDTO, CommonConstants.RISK_MANAGER,
									actionDescription);
						}
					}
					else if (loanDTO.getApplyAmountTotal().compareTo(riskAmount) < 0) {
						loanDTO.setWorkflowNextAction(
								ACMConstantWorkflowStatuts.ACTION_NOT_AUDIT_RISK);
					}
				}
			}
			else {
				// No setting REQUIRED STEP config was founded
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_NOT_AUDIT_RISK);
			}
		}
		else {
			// No setting REQUIRED STEP config was founded
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_NOT_AUDIT_RISK);
		}
		logger.info("Processing processAuditRisk() :: DONE");
		return loanDTO;
	}

	/**
	 * Notify users per group.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @param groupeCode the groupe code
	 * @param actionDescription the action description
	 * @return the loan DTO
	 */
	private LoanDTO notifyUsersPerGroup(LoanDTO loanDTO, String groupeCode,
			String actionDescription) {

		// SEND NOTIFICATION TO GROUP OF USERS
		// find users by groupe code and access branches
		List<UserDTO> userDTOs = userClient.findByGroupeCodeAndBranchIDAndAccessBranches(groupeCode,
				loanDTO.getBranchID());
		if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
			// list of users
			for (UserDTO userDTO : userDTOs) {
				// Send notif in step AUDIT
				if (groupeCode.equals(CommonConstants.BRANCH_AUDITOR)) {
					NotificationsDTO notificationsDTO =
							notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_AUDIT, "",
									loanDTO, null));
					logger.info("New Audit Notification [{}] has been inserted.", notificationsDTO);
					// Send notif in step Risk
				}
				else if (groupeCode.equals(CommonConstants.RISK_MANAGER)) {
					NotificationsDTO notificationsDTO =
							notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_RISK,
									actionDescription, loanDTO, null));
					logger.info("New Risk Notification [{}] has been inserted.", notificationsDTO);
				}
				else if (groupeCode.equals(CommonConstants.BRANCH_OPERATION)) {
					NotificationsDTO notificationsDTO =
							notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE,
									CommonConstants.ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION,
									actionDescription, loanDTO, null));
					logger.info("New Notification for BRANCH_OPERATION  [{}] has been inserted.",
							notificationsDTO);
				}

			}
		}

		return loanDTO;
	}

	/**
	 * Gets the default owner audit or risk.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the owner audit
	 */
	@SuppressWarnings("unused")
	private UserDTO getOwnerAuditOrRisk(LoanDTO loanDTO) {

		// default owner
		String owner = CommonConstants.DEFAULT_USER;
		// get users by groupe code and filter by branch of the loan
		List<UserDTO> userDTOs = new ArrayList<>();
		if (loanDTO.getEtapeWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.AUDIT).getKey()) {
			UserDTO params = new UserDTO();
			params.setGroupeCode("BRANCH_AUDITOR");
			userDTOs = userClient.findByGroupe(params);
		}
		else if (loanDTO.getEtapeWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey()) {
			UserDTO params = new UserDTO();
			params.setGroupeCode("RISK_MANAGER");
			userDTOs = userClient.findByGroupe(params);
		}
		// filtering data
		for (UserDTO userDTO : userDTOs) {
			if (!ACMValidationUtils.isNullOrEmpty(userDTO.getAccessBranches())) {
				int[] arrayBranchIds = Arrays.asList(userDTO.getAccessBranches().split(","))
						.stream().map(String::trim).mapToInt(Integer::parseInt).toArray();
				logger.info("ID Access Branches = {}", arrayBranchIds);
				Boolean exist = Boolean.FALSE;
				for (int i : arrayBranchIds) {
					if (Integer.valueOf(i) == loanDTO.getBranchID()) {
						exist = Boolean.TRUE;
					}
				}
				if (Boolean.TRUE.equals(exist)) {
					return userDTO;
				}
			}
		}
		return userClient.findByLogin(owner);
	}

	/**
	 * process Data Control CBS for loan Groupe.
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loanDTO
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 */
	private LoanDTO processDataControlCBSGroupe(LoanDTO loanDTO) throws ResourcesNotFoundException {

		// find list loan child by group loan id
		List<LoanDTO> loansChild = loanService.findByParentId(loanDTO.getLoanId());
		Boolean dataControlCBSGroupe = Boolean.TRUE;
		Boolean checkInformationLoan = Boolean.FALSE;
		Boolean checkGuarantorCollateralABACUS = Boolean.FALSE;
		Boolean checkGuarantorACM = Boolean.FALSE;
		List<String> missingData = new ArrayList<>();
		// init instant datetime
		DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy hh:mm a");
		String currentDate = dateFormat.format(new Date());
		String note = "DATA Check OK";
		if (!ACMValidationUtils.isNullOrEmpty(loansChild)) {
			for (LoanDTO loanDTOChild : loansChild) {
				// ### check Guarantor/Collateral configuration ###
				switch (loadConfigGuarantorCollateral(loanDTOChild)) {
					case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY:
						// Load grantor list from abacus DB
						List<GuarantorDTO> guarantorOnly =
								loanDetailsService.findGuarantors(loanDTOChild.getIdLoanExtern());
						// List guarantor from ACM
						CustomerLinksRelationshipDTO paramsGuarantorOnly =
								new CustomerLinksRelationshipDTO();
						paramsGuarantorOnly.setIdLoan(loanDTOChild.getLoanId());
						paramsGuarantorOnly.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						List<CustomerLinksRelationshipDTO> listGuarantorOnly =
								customerLinksRelationshipService.find(paramsGuarantorOnly);

						if (!ACMValidationUtils.isNullOrEmpty(guarantorOnly)
								&& !ACMValidationUtils.isNullOrEmpty(listGuarantorOnly)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Guarantor in the loan application. Please review.";
							missingData.add(CommonErrorCode.GUARANTOR_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_COLLATERAL_ONLY:
						// load && check list collateral from abacus DB
						List<CollaterolDTO> collaterolOnly =
								loanDetailsService.findCollaterols(loanDTOChild.getIdLoanExtern());
						if (!ACMValidationUtils.isNullOrEmpty(collaterolOnly)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Collaterals in the loan application. Please review.";
							missingData.add(CommonErrorCode.COLLATEROL_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL:
						// load guarantor list from abacus DB
						List<GuarantorDTO> guarantorDTOs =
								loanDetailsService.findGuarantors(loanDTOChild.getIdLoanExtern());

						// List guarantor from ACM
						CustomerLinksRelationshipDTO paramsGuarantorCollateral =
								new CustomerLinksRelationshipDTO();
						paramsGuarantorCollateral.setIdLoan(loanDTOChild.getLoanId());
						paramsGuarantorCollateral
								.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
						List<CustomerLinksRelationshipDTO> listGuarantorCollateral =
								customerLinksRelationshipService.find(paramsGuarantorCollateral);

						// load && check list collateral from abacus DB
						List<CollaterolDTO> collaterolDTOs =
								loanDetailsService.findCollaterols(loanDTOChild.getIdLoanExtern());
						if (!ACMValidationUtils.isNullOrEmpty(guarantorDTOs)
								&& !ACMValidationUtils.isNullOrEmpty(listGuarantorCollateral)
								&& !ACMValidationUtils.isNullOrEmpty(collaterolDTOs)) {
							checkGuarantorCollateralABACUS = Boolean.TRUE;
							checkGuarantorACM = Boolean.TRUE;
						}
						else {
							note = "Problem with data verification: Missing Guarantor or Collateral in the loan application. Please review.";
							missingData
									.add(CommonErrorCode.GUARANTOR_COLLATEROL_CODE_DATA_NOT_FOUND);
						}
						break;

					case ACMConstantWorkflowStatuts.ACTION_NOT_GUARANTOR_COLLATERAL:
						checkGuarantorCollateralABACUS = Boolean.TRUE;
						checkGuarantorACM = Boolean.TRUE;
						break;

					default:
						break;
				}
				logger.info("checkGuarantorCollateral in ABACUS DB = {}",
						checkGuarantorCollateralABACUS);
				logger.info("checkGuarantorCollateral in ACM DB = {}", checkGuarantorACM);

				// ### check Document ###
				Boolean checkDocument = acmDocumentsService.checkRequiredDocument(loanDTOChild);
				if (Boolean.FALSE.equals(checkDocument)) {
					String noteRequiredDocument =
							"Problem with data verification: Missing Required Documents in the loan application. Please review.";
					note = ACMValidationUtils.isNullOrEmpty(note) ? noteRequiredDocument
							: note + " | " + noteRequiredDocument;
					missingData.add(CommonErrorCode.UPLOAD_DOCUMENT_CODE_DATA_NOT_FOUND);
				}

				// ### check information loan ###
				// find data of the current loan in ACM DB
				LoanDTO currentLoanACM = loanService.find(loanDTOChild.getLoanId());
				// find data of the current loan in ABACUS DB
				LoanDTO currentLoanABACUS =
						transversClient.findLoan(currentLoanACM.getIdLoanExtern());
				// compare LoanDTO objects
				Map<Boolean, String> doCompareLoanMap =
						doCompareLoan(currentLoanACM, currentLoanABACUS);

				// get key from map
				Boolean key = doCompareLoanMap.entrySet().stream().findFirst().get().getKey();
				// processing data
				if (Boolean.TRUE.equals(key)) {
					checkInformationLoan = Boolean.TRUE;
				}
				else {
					// get message from map
					String message =
							doCompareLoanMap.entrySet().stream().findFirst().get().getValue();
					String noteCheckInformationLoan =
							"Problem with data verification, There are some differences in loan informations between the Core Banking and ACM : "
									+ message;
					note = ACMValidationUtils.isNullOrEmpty(note) ? noteCheckInformationLoan
							: note + " | " + noteCheckInformationLoan;
					missingData.add(CommonErrorCode.LOAN_INFORMATION_CHANGE);
				}

				// check all data is OK
				if (Boolean.FALSE.equals(checkGuarantorCollateralABACUS)
						|| Boolean.FALSE.equals(checkGuarantorACM)
						|| Boolean.FALSE.equals(checkDocument)
						|| Boolean.FALSE.equals(checkInformationLoan)) {

					dataControlCBSGroupe = Boolean.FALSE;
				}
			}
		}

		// build Note
		String noteHistorique =
				String.format("Date: %s | Added By: %s | Action: Data Verification | Note: %s",
						currentDate, loanDTO.getOwnerName(), note);
		logger.info("NoteHistorique = {}", noteHistorique);
		// check if all TRUE ==> pass to next step in process
		if (Boolean.TRUE.equals(dataControlCBSGroupe)) {
			// setting note
			loanDTO.setNote(noteHistorique);
			// update data
			updateDataValid(loanDTO);
		}
		else {
			// redirect loan to review
			updateDataNotValid(loanDTO, missingData, noteHistorique);
		}
		return loanDTO;
	}

	/**
	 * update Data if Valid.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 */
	private LoanDTO updateDataValid(LoanDTO loanDTO) throws ResourcesNotFoundException {

		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_YES);
		if (loanDTO.getEtapeWorkflow() <= 23) {
			// action in workflow schema
			loanDTO.setEtapeWorkflow(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getKey());
			loanDTO.setStatutLibelle(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getValue());
			// update status loan
			loanDTO.setStatutWorkflow(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getKey());
			loanDTO.setStatutLibelle(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT).getValue());
			// update status tab
			loanDTO.setStatut(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED).getKey());
		}
		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0).getIhmRoute());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update loan data
		LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);
		if (newLoanDTO.getParentId() == 0) {
			// Send Mail
			sendMail(new MailLoanDTO(newLoanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
					!ACMValidationUtils.isNullOrEmpty(newLoanDTO.getCustomerDTO().getEmail())
							? newLoanDTO.getCustomerDTO().getEmail()
							: defaultACMReceiverMail,
					"Your loan application has been approved : " + newLoanDTO.getAccountNumber(),
					""), MailBuilderMethod.BUILD_CUSTOMER_ACCEPT, newLoanDTO.getUpdatedBy()));
		}
		return loanDTO;
	}

	/**
	 * update Data if not Valid.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param missingData the list of missing data
	 * @param note the note
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 */
	private LoanDTO updateDataNotValid(LoanDTO loanDTO, List<String> missingData, String note)
			throws ResourcesNotFoundException {

		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_NO);
		// action in workflow schema
		loanDTO.setEtapeWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
		// loanDTO.setStatutLibelle(
		// CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getValue());
		// update status loan
		loanDTO.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REVIEW).getKey());
		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0).getIhmRoute());
		loanDTO.setListMissingData(missingData);

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// setting note
		loanDTO.setNote(note);

		// update loan data
		loanService.save(loanDTO.getLoanId(), loanDTO);

		// save historique approvel data
		LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO = new LoanApprovalHistoriqueDTO(loanDTO,
				loanDTO.getApprovelAmount().longValue(),
				CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
						.getKey(),
				note, getLastAppovelLevel(loanDTO));
		LoanApprovalHistoriqueDTO newLoanApprovalHistoriqueDTO =
				loanApprovalHistoriqueService.saveAndSetApprovalLabel(loanApprovalHistoriqueDTO);
		logger.info(
				"Loan Approval Historique with status : {} was successfully inserted for loan : {} :: RAISON = [{}]",
				newLoanApprovalHistoriqueDTO.getApprovalDesicionLabel(), loanDTO.getAccountNumber(),
				note);
		return loanDTO;
	}

	/**
	 * Action Upload Signed AgreementsGroupe.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loanDTO
	 * @throws UploadDocumentNotFoundException the Upload Doc Not Found Exepction
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	private LoanDTO actionUploadDocumentGroupe(LoanDTO loanDTO)
			throws UploadDocumentNotFoundException, ResourcesNotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			DisbursementException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		Boolean resultatCheck = Boolean.FALSE;
		logger.info("Begin actionUploadDocumentGroupe method...");
		// load list of document by loan
		List<LoanDTO> loansChild = loanService.findByParentId(loanDTO.getLoanId());
		if (!ACMValidationUtils.isNullOrEmpty(loansChild)) {
			for (LoanDTO loanDTOChild : loansChild) {
				loanDTOChild.setChildMissingInfo(Boolean.FALSE);
				// check Required Document
				loanDTOChild.setChildMissingInfo(
						acmDocumentsService.checkRequiredDocument(loanDTOChild));
			}
			List<Boolean> missingDocInChilds = new ArrayList<>();
			loansChild.stream()
					.forEach(loanChild -> missingDocInChilds.add(loanChild.getChildMissingInfo()));
			if (missingDocInChilds.contains(Boolean.FALSE)) {
				resultatCheck = Boolean.TRUE;
			}
			if (Boolean.FALSE.equals(resultatCheck)) {
				// complete workflow for all childs
				loanService.completeWorkflowForChilds(loanDTO, loansChild);

				// update statutWorkflow
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getValue());
				// update EtapeWorkflow
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.SCREENING).getKey());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());
				// update next action
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_YES);

				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_SCREENING)).get(0)
						.getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				loanService.save(loanDTO.getLoanId(), loanDTO);
			}
			else {
				// complete workflow for all childs
				loanService.completeWorkflowForChilds(loanDTO, loansChild);
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_NO);
			}
			// update loan data
			loanService.save(loanDTO.getLoanId(), loanDTO);
		}
		else {
			logger.error("Failed to execute actionUploadDocument !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new UploadDocumentNotFoundException(
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty(
							"workflow.exception.message.not.found.actionUploadDocument")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		logger.info("Complete to execute actionUploadDoc !!!");
		return loanDTO;
	}

	/**
	 * Gets the user by portfolio id otherwise returning default user.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the username by portfolio id
	 */
	private UserDTO getUsernameByPortfolioId(LoanDTO loanDTO) {

		// default ADMIN user
		String userName = CommonConstants.DEFAULT_USER;
		// find user by AccountPortfolioId
		if (loanDTO.getPortfolioId() != null && loanDTO.getPortfolioId() != 0) {
			UserDTO params = new UserDTO();
			params.setAccountPortfolioId(loanDTO.getPortfolioId());
			List<UserDTO> userDTOs = userClient.find(params);
			// check && setting
			if (!ACMValidationUtils.isNullOrEmpty(userDTOs)) {
				// setting userName
				return userDTOs.get(0);
			}
		}
		logger.info(" founded USER_NAME = [{}]", userName);
		return userClient.findByLogin(userName);
	}

	/**
	 * Send mail.
	 * 
	 * @author HaythemBenizid
	 * @param mailLoanDTO the loanMail DTO
	 */
	private void sendMail(MailLoanDTO mailLoanDTO) {

		try {
			mailSenderClient.sendEmail(mailLoanDTO);
			logger.info("Sending Email Notification :: DONE");
		}
		catch (Exception e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/**
	 * Notify one user per group.
	 * 
	 * @author ManelLamloum
	 * @param loanDTO the loan DTO
	 * @param groupeCode the groupe code
	 * @param actionDescription the action description
	 * @return the loan DTO
	 */
	private LoanDTO notifyOneUserPerGroup(LoanDTO loanDTO, String groupeCode,
			String actionDescription) {

		// SEND NOTIFICATION TO THE USER TO WHICH THE LOAN IS ASSIGNED
		// Send notif in step AUDIT
		if (groupeCode.equals(CommonConstants.BRANCH_AUDITOR)) {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_AUDIT,
							actionDescription, loanDTO, null));
			logger.info("New Audit Notification [{}] has been inserted.", notificationsDTO);

		} // Send notif in step Risk
		else if (groupeCode.equals(CommonConstants.RISK_MANAGER)) {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_RISK,
							actionDescription, loanDTO, null));
			logger.info("New Risk Notification [{}] has been inserted.", notificationsDTO);
		}
		else if (groupeCode.equals(CommonConstants.BRANCH_OPERATION)) {

			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION,
							actionDescription, loanDTO, null));
			logger.info("New Notification for BRANCH_OPERATION  [{}] has been inserted.",
					notificationsDTO);
		}
		return loanDTO;
	}
}
