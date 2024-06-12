/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.aop.history.ProcessHistoryLoan;
import com.acm.client.GedClient;
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
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CheckAppL1NotFoundException;
import com.acm.exceptions.type.CheckAppL2NotFoundException;
import com.acm.exceptions.type.CheckAppL3NotFoundException;
import com.acm.exceptions.type.CheckAppL4NotFoundException;
import com.acm.exceptions.type.CheckApprovelLevelException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CollateralNotFoundException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.FieldVisitNotFoundException;
import com.acm.exceptions.type.GuarantorsNotFoundException;
import com.acm.exceptions.type.InformCustomerNotFoundException;
import com.acm.exceptions.type.InitialCheckException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.UploadSignedDocNotFoundExepction;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.service.AcmCollateralService;
import com.acm.service.AcmDocumentsService;
import com.acm.service.CustomerDecisionService;
import com.acm.service.CustomerLinksRelationshipService;
import com.acm.service.CustomerService;
import com.acm.service.ExceptionRequestService;
import com.acm.service.LoanApprovalHistoriqueService;
import com.acm.service.LoanInstanceService;
import com.acm.service.LoanParticipantsService;
import com.acm.service.LoanService;
import com.acm.service.LoanWorkflowSystemService;
import com.acm.service.LoanWorkflowUserActionService;
import com.acm.service.NotificationsServices;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.CustomerDecisionDTO;
import com.acm.utils.dtos.CustomerLinksRelationshipDTO;
import com.acm.utils.dtos.ExceptionRequestDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.dtos.LoanApprovalHistoriqueDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldsLinksDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.LinkRelationshipsCategory;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.models.Loan;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import feign.FeignException;

/**
 * {@link LoanWorkflowUserActionServiceImpl} class The implementation of the service
 * {@link LoanWorkflowUserActionService}.
 *
 * @author HaythemBenizid
 * @since 0.6.0
 */
@Service
public class LoanWorkflowUserActionServiceImpl implements LoanWorkflowUserActionService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(LoanWorkflowUserActionServiceImpl.class);

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The loan approval historique service. */
	@Autowired
	private LoanApprovalHistoriqueService loanApprovalHistoriqueService;

	/** The customer decision service. */
	@Autowired
	private CustomerDecisionService customerDecisionService;

	/** The acm documents service. */
	@Autowired
	private AcmDocumentsService acmDocumentsService;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The loan instance service. */
	@Autowired
	private LoanInstanceService loanInstanceService;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The customer links relationship service. */
	@Autowired
	private CustomerLinksRelationshipService customerLinksRelationshipService;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The userClient. */
	@Autowired
	private UserClient userClient;

	/** The exception request service. */
	@Autowired
	private ExceptionRequestService exceptionRequestService;
	/** The environment. */
	@Autowired
	private Environment environment;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The acm collateral service. */
	@Autowired
	AcmCollateralService acmCollateralService;
	/** The loan workflow system service. */
	@Autowired
	private LoanWorkflowSystemService loanWorkflowSystemService;

	/** The Constant SUBJECT_MAIL. */
	private static final String SUBJECT_MAIL = "Your loan application : ";

	/**
	 * Action dynamic workflow.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionDynamicWorkflow(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@ProcessHistoryLoan(action = CommonAOPConstants.DYNAMIC_WF_ACTION)
	@Override
	public LoanDTO actionDynamicWorkflow(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin actionDynamicWorkflow method...");
		if (!ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {

			// setting date change status
			loanDTO.setChangeDateStatusWorkflow(new Date());
			loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

			// update list participants
			loanParticipantsService
					.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

			return loanDTO;
		}
		return null;
	}

	/**
	 * Action initial check.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws InitialCheckException the initial check exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionInitialCheck(com.acm.utils.dtos.LoanDTO)
	 */
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_INITIAL_CHECK)
	@Override
	public LoanDTO actionInitialCheck(LoanDTO loanDTO) throws InitialCheckException,
			ResourcesNotFoundException, ApiAbacusException, IOException {

		logger.info("Begin actionInitialCheck method method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionInitialCheck !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new InitialCheckException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment
							.getProperty("workflow.exception.message.not.found.actionInitialCheck")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}

		if (loanDTO.getWorkflowNextAction()
				.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT)) {
			// action in workflow schema
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_ACCEPTED);

			// update status loan
			loanDTO.setStatutWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey());
			loanDTO.setStatutLibelle(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getValue());
			loanDTO.setEtapeWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.FIELD_VISIT).getKey());
			loanDTO.setIhmRoot(parametrageClient
					.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_FIELD_VISIT)).get(0)
					.getIhmRoute());
			// update status tab
			loanDTO.setStatut(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());

			// setting date change status
			loanDTO.setChangeDateStatusWorkflow(new Date());
			loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		}
		else if (loanDTO.getWorkflowNextAction()
				.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REJECT)) {
			if (Boolean.TRUE.equals(loanDTO.getConfirm())) {
				// action in workflow schema
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_REJECTED);

				// update status loan
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getValue());
				// update status tab
				loanDTO.setStatut(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REJECTED).getKey());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
			}
			else {
				logger.error("Failed to execute actionInitialCheckReject !!!");
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new InitialCheckException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment.getProperty(
								"workflow.exception.message.not.found.actionInitialCheck")
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());
			}

		}

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);

		loanWorkflowSystemService.processEtatRejete(loanDTO);

		// update loan data
		LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);
		logger.info("Complete to execute actionInitialCheck !!!");
		// check guarantor collateral setting
		LoanDTO newLoanDTOProcess =
				loanWorkflowSystemService.processCheckGuarantorCollateral(newLoanDTO);
		return newLoanDTOProcess;
	}

	/**
	 * Action field visit.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws FieldVisitNotFoundException the field visit not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionFieldVisit(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_FIELD_VISIT)
	public LoanDTO actionFieldVisit(LoanDTO loanDTO)
			throws ResourcesNotFoundException, FieldVisitNotFoundException {

		logger.info("Begin actionFieldVisit method...");
		// update loan data
		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_FIELD_VISIT);
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionFieldVisit !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new FieldVisitNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.actionFieldVisit")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		LoanDTO newLoanDTOProcess = loanWorkflowSystemService.processAuditRisk(loanDTO);
		logger.info("Complete to execute actionFieldVisit !!!");
		return newLoanDTOProcess;
	}

	/**
	 * Action check guarantor.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws GuarantorsNotFoundException the guarantors not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckGuarantor(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_GUARANTOR)
	public LoanDTO actionCheckGuarantor(LoanDTO loanDTO)
			throws GuarantorsNotFoundException, ResourcesNotFoundException {

		logger.info("Begin actionCheckGuarantor method...");
		// check application fee paid or not
		AcmEnvironnementDTO feeEnvironnement = parametrageClient.find("APPLICATION_FEE");
		if (!ACMValidationUtils.isNullOrEmpty(feeEnvironnement)
				&& feeEnvironnement.getValue().equals("1")) {
			Long applicationFee = transversClient.findApplicationFee(loanDTO.getIdAccountExtern());
			if (applicationFee == 0) {
				logger.error("Failed to find Paid Application Fee !!!");
				throw new GuarantorsNotFoundException(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						"Failed to find Paid Application Fee" + Loan.class.getSimpleName()
								+ CommonExceptionsMessage.WITH_ID + loanDTO.getAccountNumber());
			}
		}

		// add Guarantors !!!
		addGuarantor(loanDTO);
		// check Guarantor/Collateral configuration
		String resultatCheck = ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY;
		// find config by product ID & mandatory column is TRUE
		List<SettingGurantorCollateralDTO> settingGurantorCollateralDTOs = parametrageClient
				.find(new SettingGurantorCollateralDTO(loanDTO.getProductId(), Boolean.TRUE));
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTOs)) {
			List<String> settings = new ArrayList<>();
			settingGurantorCollateralDTOs.stream().forEach(
					settingGurantorCollateral -> settings.add(settingGurantorCollateral.getCode()));
			if (settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL)) {
				resultatCheck = ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL;
			}
		}
		switch (resultatCheck) {
			// ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY
			case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_ONLY:
				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
				loanDTO.setStatutLibelle(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getValue());
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_UPLOAD_DOCUMENT))
						.get(0).getIhmRoute());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			// ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL
			case ACMConstantWorkflowStatuts.ACTION_GUARANTOR_COLLATERAL:

				loanDTO.setStatutWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.COLLATERAL).getKey());
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

			default:
				break;
		}

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		return loanDTO;
	}

	/**
	 * Action check collateral.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CollateralNotFoundException the collateral not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckCollateral(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_COLLATERAL)
	public LoanDTO actionCheckCollateral(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CollateralNotFoundException {

		logger.info("Begin actionCheckCollateral method...");
		// load && check list collateral from abacus DB
		AcmCollateralDTO acmCollateralDTO = new AcmCollateralDTO();
		acmCollateralDTO.setLoan(loanDTO);
		List<AcmCollateralDTO> collaterolDTOs = acmCollateralService.find(acmCollateralDTO);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(collaterolDTOs)) {
			logger.error("Failed to execute actionCheckCollateral !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CollateralNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.collateral")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		// update status loan and passing to add document step
		if (loanDTO.getEtapeWorkflow() <= 23) {
			loanDTO.setStatutWorkflow(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
			loanDTO.setEtapeWorkflow(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getKey());
			loanDTO.setStatutLibelle(CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.ADD_DOCUMENTS).getValue());
		}

		loanDTO.setIhmRoot(parametrageClient
				.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_IHM_UPLOAD_DOCUMENT)).get(0)
				.getIhmRoute());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);

		// update loan data
		return loanService.save(loanDTO.getLoanId(), loanDTO);
	}

	/**
	 * Action upload documents.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionUploadDocuments(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_UPLOAD_DOCUMENTS)
	public LoanDTO actionUploadDocuments(LoanDTO loanDTO)
			throws DisbursementException, WorkFlowSettingException, CheckMezaCardException,
			CheckMezaCardUntrustException, CheckFeesException {

		logger.info("Begin actionUploadDocuments method...");
		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		logger.info("Complete to execute actionUploadDocuments !!!");
		LoanDTO newLoanDTO = new LoanDTO();
		try {
			newLoanDTO = loanWorkflowSystemService.processCheckEtatRequiredDocs(loanDTO);
		}
		catch (CheckApprovelLevelException | ApiAbacusException | CreditException
				| ResourcesNotFoundException | UploadDocumentNotFoundException | IOException e) {
			e.printStackTrace();
		}
		return newLoanDTO;
	}

	/**
	 * Action add financial analysis.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionAddFinancialAnalysis(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_ADD_FINANCIAL_ANALYSIS)
	public LoanDTO actionAddFinancialAnalysis(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin actionAddFinancialAnalysis method...");
		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		logger.info("Complete to execute actionAddFinancialAnalysis !!!");
		return loanDTO;
	}

	/**
	 * Action check L 1.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckL1(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_L1)
	public LoanDTO actionCheckL1(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException {

		logger.info("Begin actionCheckL1 method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionCheckL1 !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CheckAppL1NotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.actionCheckL1")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		else {
			actionLevel(loanDTO,
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LEVEL1).getKey());
		}
		logger.info("Complete to execute actionCheckL1 !!!");
		return loanDTO;
	}

	/**
	 * Action check L 2.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckL2(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_L2)
	public LoanDTO actionCheckL2(LoanDTO loanDTO)
			throws CheckAppL2NotFoundException, ResourcesNotFoundException,
			CheckAppL1NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException {

		logger.info("Begin actionCheckL2 method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionCheckL2 !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CheckAppL2NotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.actionCheckL2")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		else {
			actionLevel(loanDTO,
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LEVEL2).getKey());
		}
		logger.info("Complete to execute actionCheckL2 !!!");
		return loanDTO;
	}

	/**
	 * Action check L 3.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckL3(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_L3)
	public LoanDTO actionCheckL3(LoanDTO loanDTO)
			throws CheckAppL3NotFoundException, ResourcesNotFoundException,
			CheckAppL1NotFoundException, CheckAppL2NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException {

		logger.info("Begin actionCheckL3 method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionCheckL3 !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CheckAppL3NotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.actionCheckL3")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		else {
			actionLevel(loanDTO,
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LEVEL3).getKey());
		}
		logger.info("Complete to execute actionCheckL3 !!!");
		return loanDTO;
	}

	/**
	 * Action check L 4.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCheckL4(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CHECK_L4)
	public LoanDTO actionCheckL4(LoanDTO loanDTO)
			throws CheckAppL4NotFoundException, ResourcesNotFoundException,
			CheckAppL1NotFoundException, CheckAppL2NotFoundException, CheckAppL3NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException {

		logger.info("Begin actionCheckL4 method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionCheckL4 !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new CheckAppL4NotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("workflow.exception.message.not.found.actionCheckL4")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		else {
			actionLevel(loanDTO,
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.LEVEL4).getKey());
		}
		logger.info("Complete to execute actionCheckL4 !!!");
		return loanDTO;
	}

	/**
	 * Save loan approval historique by given status : (REJECTED / APPROVED / REVIEW) .
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param statusId the status id
	 */
	private void saveLoanApprovalHistorique(LoanDTO loanDTO, Integer statusId) {

		// save historique approvel data
		LoanApprovalHistoriqueDTO loanApprovalHistoriqueDTO =
				new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
						statusId, loanDTO.getNote(), loanDTO.getStatutWorkflow());
		LoanApprovalHistoriqueDTO newLoanApprovalHistoriqueDTO =
				loanApprovalHistoriqueService.saveAndSetApprovalLabel(loanApprovalHistoriqueDTO);
		logger.info(
				"Loan Approval Historique with status : {} was successfully inserted for loan :  {}",
				newLoanApprovalHistoriqueDTO.getApprovalDesicionLabel(),
				loanDTO.getAccountNumber());
	}

	/**
	 * action Level.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param etapeLevel the etapeLevel : (Level 1 / Level 2 / Level 3/ Level 4)
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private void actionLevel(LoanDTO loanDTO, Integer etapeLevel)
			throws ResourcesNotFoundException, CheckAppL1NotFoundException,
			CheckAppL2NotFoundException, CheckAppL3NotFoundException, CheckAppL4NotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException {

		switch (loanDTO.getWorkflowNextAction()) {
			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REJECT:
				if (Boolean.TRUE.equals(loanDTO.getConfirm())) {
					loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_REJECTED);
					// update status loan
					loanDTO.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey());
					loanDTO.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getValue());
					// update status tab
					loanDTO.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REJECTED)
							.getKey());
					loanDTO.setIhmRoot(parametrageClient
							.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
							.getIhmRoute());

					// setting date change status
					loanDTO.setChangeDateStatusWorkflow(new Date());
					loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

					// update list participants
					loanParticipantsService.update(
							new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

					// assign to default ACCOUNTPORTFOLIO_CODE
					UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
					loanDTO.setOwner(foundedUserDTO.getLogin());
					loanDTO.setOwnerName(foundedUserDTO.getSimpleName());
					// setting list participants
					LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
							.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
					logger.info("(actionLevel) setting Loan Participants  with ID = [{}] :: DONE",
							newLoanParticipantsDTO.getId());

					// save historique approvel data for individual loan
					if (loanDTO.getParentId() == 0) {
						saveLoanApprovalHistorique(loanDTO,
								CommonFunctions.mappingStatus(
										ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REJECTED)
										.getKey());
					}

					// update loan data
					LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);

					// Processing Loan in ACTIVITI
					// activitiProcessService.process(loanDTO);

					loanWorkflowSystemService.processEtatRejete(loanDTO);

					// Send Mail
					sendMail(new MailLoanDTO(newLoanDTO,
							new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
									newLoanDTO.getCustomerDTO().getEmail(),
									SUBJECT_MAIL + newLoanDTO.getAccountNumber(), ""),
							MailBuilderMethod.BUILD_CUSTOMER_REJECT, newLoanDTO.getUpdatedBy()));
				}
				else {
					checkExeptionLevel(loanDTO, etapeLevel,
							CommonFunctions.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REJECTED)
									.getValue());
				}
				break;

			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE:
				if (Boolean.TRUE.equals(loanDTO.getConfirm())) {
					// if loan status = approvel L4 send to approve
					if (loanDTO.getStatutWorkflow() == CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L4).getKey()) {
						loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_APPROVED);
					}
					else {
						loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_ACCEPTED);
					}

					loanDTO.setIhmRoot(parametrageClient
							.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
							.getIhmRoute());
					// update loan in ABACUS
					try {
						if (loanDTO.getUpdateLoanAbacus()) {
							// update to abacus via API
							transversClient.updateLoanINDIVORG(loanDTO);
						}
					}
					catch (Exception e) {
						logger.error("Failed to add loan {}", e.getMessage());
						logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
								e.getMessage());
						// INIT error message
						String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
						if (e != null && e.getMessage() != null
								&& e.getMessage().contains("errorMessage")) {
							String msgFromTransversApi =
									e.getMessage().substring(e.getMessage().indexOf('{'));
							final JsonNode jsonNode =
									new ObjectMapper().readTree(msgFromTransversApi);
							messageError = jsonNode.get("errorMessage").asText();
						}
						// Fire Exception
						throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
					}
					// update loan data
					LoanDTO updatedLoan = loanService.save(loanDTO.getLoanId(), loanDTO);
					// check if approvel amount is change
					if (updatedLoan.getApplyAmountTotal()
							.compareTo(updatedLoan.getApprovelAmount()) != 0) {
						// updating the process for given loan
						// List<LoanInstanceDTO> newLoanInstanceDTOs =
						// loanInstanceService.updateForWorkflow(updatedLoan);

					}

					// save historique approvel data
					loanDTO.setNote(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
							.getValue());
					if (loanDTO.getParentId() == 0) {
						saveLoanApprovalHistorique(loanDTO,
								CommonFunctions.mappingStatus(
										ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
										.getKey());
					}
					loanWorkflowSystemService.processCheckApprobationLevel(loanDTO);
					// Processing Loan in ACTIVITI
					// activitiProcessService.process(loanDTO);
				}
				else {
					checkExeptionLevel(loanDTO, etapeLevel,
							CommonFunctions.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_APPROVED)
									.getValue());
				}
				break;

			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW:
				if (Boolean.TRUE.equals(loanDTO.getConfirm())) {
					// process review step
					loanDTO = actionCorrectifs(loanDTO);

					// update loan data
					loanService.save(loanDTO.getLoanId(), loanDTO);
					break;
				}
				else {
					checkExeptionLevel(loanDTO, etapeLevel,
							CommonFunctions
									.mappingStatus(
											ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
									.getValue());
				}
				break;

			default:
				break;
		}
	}

	/**
	 * checkExeptionLevel.
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param etapeLevel the etapeLevel : (Level 1 / Level 2 / Level 3/ Level 4)
	 * @param actionWorkflow the actionWorkflow : (Reject / Approval / Review)
	 * @throws CheckAppL1NotFoundException the checkAppL1 not found exception
	 * @throws CheckAppL2NotFoundException the checkAppL2 not found exception
	 * @throws CheckAppL3NotFoundException the checkAppL3 not found exception
	 * @throws CheckAppL4NotFoundException the checkAppL4 not found exception
	 */
	private void checkExeptionLevel(LoanDTO loanDTO, Integer etapeLevel, String actionWorkflow)
			throws CheckAppL1NotFoundException, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException {

		switch (etapeLevel) {
			case 1:
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new CheckAppL1NotFoundException(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment
								.getProperty("workflow.exception.message.not.found.actionCheckL1"
										+ actionWorkflow)
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());

			case 2:
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new CheckAppL2NotFoundException(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment
								.getProperty("workflow.exception.message.not.found.actionCheckL2"
										+ actionWorkflow)
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());

			case 3:
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new CheckAppL3NotFoundException(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment
								.getProperty("workflow.exception.message.not.found.actionCheckL3"
										+ actionWorkflow)
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());

			case 4:
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new CheckAppL4NotFoundException(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment
								.getProperty("workflow.exception.message.not.found.actionCheckL4"
										+ actionWorkflow)
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());
			default:
				break;
		}
	}

	/**
	 * Action inform customer.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ApiAbacusException the api abacus exception
	 * @throws CheckAppL1NotFoundException the check app L 1 not found exception
	 * @throws CheckAppL2NotFoundException the check app L 2 not found exception
	 * @throws CheckAppL3NotFoundException the check app L 3 not found exception
	 * @throws CheckAppL4NotFoundException the check app L 4 not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionInformCustomer(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_INFORM_CUSTOMER)
	public LoanDTO actionInformCustomer(LoanDTO loanDTO) throws ResourcesNotFoundException,
			InformCustomerNotFoundException, CheckApprovelLevelException, IOException,
			ApiAbacusException, CheckAppL1NotFoundException, CheckAppL2NotFoundException,
			CheckAppL3NotFoundException, CheckAppL4NotFoundException {

		logger.info("Begin actionInformCustomer method...");
		if (ACMValidationUtils.isNullOrEmpty(loanDTO.getWorkflowNextAction())) {
			logger.error("Failed to execute actionInformCustomer !!!");
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
			throw new InformCustomerNotFoundException(
					CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty(
							"workflow.exception.message.not.found.actionInformCustomer")
							+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
							+ loanDTO.getAccountNumber());
		}
		else {
			switch (loanDTO.getWorkflowNextAction()) {
				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_DECLINED:
					if (Boolean.TRUE.equals(loanDTO.getConfirm())) {
						loanDTO.setWorkflowNextAction(
								ACMConstantWorkflowStatuts.ACTION_CUSTOMER_AGREED_NO);

						// update loan data
						LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);
						// Processing Loan in ACTIVITI
						// newLoanDTO = activitiProcessService.process(loanDTO);
						newLoanDTO = loanWorkflowSystemService.processEtatCanceled(loanDTO);
						// save customer decision
						saveCustomerDecision(loanDTO, CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_DECLIEND)
								.getKey());
						// Send Mail
						sendMail(new MailLoanDTO(newLoanDTO,
								new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
										newLoanDTO.getCustomerDTO().getEmail(),
										SUBJECT_MAIL + newLoanDTO.getAccountNumber(), ""),
								MailBuilderMethod.BUILD_CUSTOMER_DECLINE,
								newLoanDTO.getUpdatedBy()));
					}
					else {
						logger.error("Failed to execute actionInformCustomer !!!");
						logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
								Loan.class.getSimpleName());
						throw new InformCustomerNotFoundException(
								CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
								environment.getProperty(
										"workflow.exception.message.not.found.actionInformCustomer")
										+ Loan.class.getSimpleName()
										+ CommonExceptionsMessage.WITH_ID
										+ loanDTO.getAccountNumber());
					}
					break;

				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_AGREED:

					// ### update application data && lunch APPROVAL process = 1 ###
					if (loanDTO.getParentId() == 0) {
						// find product object by ID
						loanDTO.setProductDTO(parametrageClient
								.findProductById(loanDTO.getProductId().longValue()));
						// find UDF for loan
						UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
						params.setElementId(loanDTO.getLoanId());
						params.setCategory(CommonConstants.LOAN_CATEGORY);
						loanDTO.setUserDefinedFieldsLinksDTOs(
								userDefinedFieldsLinksService.find(params));
						try {
							// update && call Approval API
							updateAndApprovalLoan(loanDTO, 1);
						}
						catch (Exception e) {
							logger.error(
									"Failed to execute method updateAndApprovalLoan in actionInformCustomer() : {}",
									e.getMessage());
							// INIT error message
							String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
							if (e != null && e.getMessage() != null
									&& e.getMessage().contains("errorMessage")) {
								String msgFromTransversApi =
										e.getMessage().substring(e.getMessage().indexOf('{'));
								final JsonNode jsonNode =
										new ObjectMapper().readTree(msgFromTransversApi);
								messageError = jsonNode.get("errorMessage").asText();
							}
							// Fire Exception
							throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
						}
					}
					// update loan data
					loanDTO.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.ACTION_CUSTOMER_AGREED_YES);
					loanService.save(loanDTO.getLoanId(), loanDTO);

					// Processing Loan in ACTIVITI
					// activitiProcessService.process(loanDTO);
					// save customer decision will be done in processDataControlCBS() method
					loanWorkflowSystemService.processDataControlCBS(loanDTO);

					break;

				case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_ASK_FOR_REVIEW:
					loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_REVIEW);
					if (loanDTO.getEtapeWorkflow() <= 23) {
						// action in workflow schema
						loanDTO.setEtapeWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey());
						loanDTO.setStatutLibelle(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getValue());
						// update status loan
						loanDTO.setStatutWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey());

						// update status tab
						loanDTO.setStatut(CommonFunctions
								.mappingStatus(
										ACMConstantWorkflowStatuts.STATUS_TAB_PENDING_APPROVAL)
								.getKey());
						loanDTO.setIhmRoot(parametrageClient
								.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
								.getIhmRoute());
					}

					// setting date change status
					loanDTO.setChangeDateStatusWorkflow(new Date());
					loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
					// assign to default responsable.
					UserDTO userConnect = getUsernameByPortfolioId(loanDTO);
					if (!userConnect.getResponsableId().equals("0")) {
						loanDTO.setOwner(userConnect.getResponsableId());
						loanDTO.setOwnerName(userClient.findByLogin(userConnect.getResponsableId())
								.getSimpleName());
					}
					// update loan data
					loanService.save(loanDTO.getLoanId(), loanDTO);
					// activitiProcessService.process(loanDTO);
					if (loanDTO.getEtapeWorkflow() <= 23) {
						actionCheckL1(loanDTO);
					}
					// save customer decision
					saveCustomerDecision(loanDTO,
							CommonFunctions.mappingStatus(
									ACMConstantWorkflowStatuts.CUSTOMER_DESICION_STATUS_ASK_REVIEW)
									.getKey());
					break;

				default:
					break;
			}
		}

		logger.info("Complete to execute actionInformCustomer !!!");
		return loanDTO;
	}

	/**
	 * Sets the loan to group of users.
	 * 
	 * @author idridi
	 * @param groupeCode the groupe code
	 * @param loanDTO the loan DTO
	 */
	private void setLoanToGroupOfUsers(String groupeCode, LoanDTO loanDTO) {

		// update loan data
		// find group by code group
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
			// set group owner and group owner name
			loanDTO.setGroupOwner(groupeDTO.getCode());
			loanDTO.setGroupOwnerName(groupeDTO.getLibelle());
			loanDTO.setAssignedToOneUser(Boolean.FALSE);
		}
	}

	/**
	 * Update application data and Approval LOAN via ABACUS API.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param levelApproval the level approval
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws ApiAbacusException the api abacus exception
	 */
	private void updateAndApprovalLoan(LoanDTO loanDTO, Integer levelApproval)
			throws IOException, ResourcesNotFoundException, ApiAbacusException {

		logger.info("LOAN APPROVAL Process IN ABACUS via API : LEVEL [{}]", levelApproval);
		try {
			// init first level process step
			loanDTO.setLoanApprovalLevel(levelApproval);
			if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				List<LoanDTO> loanDTOs = new ArrayList<>();
				loanDTOs.add(loanDTO);
				// load group childs
				List<LoanDTO> loansChilds = loanService.findByParentId(loanDTO.getLoanId());
				// setting approval level for all childs
				for (LoanDTO child : loansChilds) {
					child.setLoanApprovalLevel(levelApproval);
					child.setProductDTO(loanDTO.getProductDTO());
					// add child
					loanDTOs.add(child);
				}
				// find customer object
				CustomerDTO customerDTO = customerService.find(loanDTO.getCustomerDTO().getId());

				// update CUSTOMER data via API ABACUS
				transversClient.updateCustomer(customerDTO);

				// Add GUARANTOR data via API ABACUS
				// case of normal loan (not topup)
				if (levelApproval == 1 && loanDTO.getLoanApplicationStatus()
						.equals(CommonConstants.NEW_APPLICATION)) {
					CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
					params.setIdLoan(loanDTO.getLoanId());
					params.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
					List<CustomerLinksRelationshipDTO> existGuarantors =
							customerLinksRelationshipService.find(params);
					// check guarantor exist in ABACUS DB
					if (!ACMValidationUtils.isNullOrEmpty(existGuarantors)) {
						List<GuarantorDTO> guarantorDTOs = new ArrayList<>();
						for (CustomerLinksRelationshipDTO guarantorParam : existGuarantors) {
							if (guarantorParam.getMember() != null) {
								// init id customer extern
								Long idCustomerIdExtern =
										guarantorParam.getMember().getCustomerIdExtern();
								// check customer exist in ABACUS
								if (idCustomerIdExtern == 0) {
									// add guarantor as customer in ABACUS DB
									CustomerDTO guarantor =
											transversClient.addCustomer(guarantorParam.getMember());
									idCustomerIdExtern = guarantor.getCustomerIdExtern();
								}
								// setting Customer Id Extern
								guarantorDTOs.add(new GuarantorDTO(loanDTO.getIdLoanExtern(),
										loanDTO.getIdAccountExtern(), idCustomerIdExtern, "", null,
										1L, guarantorParam.getAmountGuarantor(), 0L));

								// find and save loan collateral in ABACUS via API

								AcmCollateralDTO paramCollateral = new AcmCollateralDTO();
								paramCollateral.getLoan()
										.setIdLoanExtern(loanDTO.getIdLoanExtern());
								List<AcmCollateralDTO> acmCollateralDTOs =
										acmCollateralService.find(paramCollateral);
								if (!ACMValidationUtils.isNullOrEmpty(acmCollateralDTOs)) {
									loanDTO.setCollaterals(acmCollateralDTOs);
									transversClient.createLoanCollateral(loanDTO);
								}
							}
						}
						logger.info("guarantors to be saved in abacus : {}", guarantorDTOs);
						transversClient.createGuarantors(guarantorDTOs);
					}
				}
				// Send APPROVAL && update API
				transversClient.approvelGroup(loanDTOs);
			}
			else {
				// find customer object
				CustomerDTO customerDTO = customerService.find(loanDTO.getCustomerDTO().getId());
				// update CUSTOMER data via API ABACUS
				transversClient.updateCustomer(customerDTO);
				// Add GUARANTOR data via API ABACUS
				// case of normal loan (not topup)
				if (levelApproval == 1 && loanDTO.getLoanApplicationStatus()
						.equals(CommonConstants.NEW_APPLICATION)) {
					CustomerLinksRelationshipDTO params = new CustomerLinksRelationshipDTO();
					params.setIdLoan(loanDTO.getLoanId());
					params.setCategory(LinkRelationshipsCategory.GUARANTOR.name());
					List<CustomerLinksRelationshipDTO> existGuarantors =
							customerLinksRelationshipService.find(params);
					// check guarantor exist in ABACUS DB
					if (!ACMValidationUtils.isNullOrEmpty(existGuarantors)) {
						List<GuarantorDTO> guarantorDTOs = new ArrayList<>();
						for (CustomerLinksRelationshipDTO guarantorParam : existGuarantors) {
							if (existGuarantors.get(0).getMember() != null) {
								// init id customer extern
								Long idCustomerIdExtern =
										guarantorParam.getMember().getCustomerIdExtern();
								// check customer exist in ABACUS
								if (idCustomerIdExtern == 0) {
									// add guarantor as customer in ABACUS DB
									CustomerDTO guarantor =
											transversClient.addCustomer(guarantorParam.getMember());
									idCustomerIdExtern = guarantor.getCustomerIdExtern();
								}
								// setting Customer Id Extern
								guarantorDTOs.add(new GuarantorDTO(loanDTO.getIdLoanExtern(),
										loanDTO.getIdAccountExtern(), idCustomerIdExtern, "", null,
										1L, guarantorParam.getAmountGuarantor(), 0L));

								// find and save loan collateral in ABACUS via API

								AcmCollateralDTO paramCollateral = new AcmCollateralDTO();
								paramCollateral.setLoan(new LoanDTO());
								paramCollateral.getLoan()
										.setIdLoanExtern(loanDTO.getIdLoanExtern());
								List<AcmCollateralDTO> acmCollateralDTOs =
										acmCollateralService.find(paramCollateral);
								if (!ACMValidationUtils.isNullOrEmpty(acmCollateralDTOs)) {
									loanDTO.setCollaterals(acmCollateralDTOs);

									transversClient.createLoanCollateral(loanDTO);
								}
							}
						}
						logger.info("guarantors to be saved in abacus : {}", guarantorDTOs);
						transversClient.createGuarantors(guarantorDTOs);
					}
				}
				// get initial payment date from the setting
				Integer maxIssueDate =
						Integer.parseInt(parametrageClient.find("ISSUE_DATE").getValue());
				Integer dateRepaymentDate =
						Integer.parseInt(parametrageClient.find("INITIAL_PAYMENT").getValue());
				if (maxIssueDate != 0 && dateRepaymentDate != 0) {
					loanDTO.setInitialPaymentDate(Date.from(
							loanService.calculateFirstRepaymentDate(maxIssueDate, dateRepaymentDate)
									.atStartOfDay(ZoneId.systemDefault()).toInstant()));
				}
				loanDTO.setUserDefinedFieldsLinksDTOs(loanDTO.getUserDefinedFieldsLinksDTOs()
						.stream()
						.filter(udfLink -> !ACMValidationUtils
								.isNullOrEmpty(udfLink.getUserDefinedFieldsDTO().getIdUDFField()))
						.collect(Collectors.toList()));
				// Send approval && update API loan ORG or INDIV
				transversClient.approveLoan(loanDTO);
			}
		}
		catch (Exception e) {
			logger.error("Failed to APPROVE LOAN in ABACUS : {}", e);
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
	}

	/**
	 * Save customer decision by given status : (REJECTED / APPROVED / REVIEW / REVIEW_AGREEMNETS) .
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param statusId the status id
	 */
	private void saveCustomerDecision(LoanDTO loanDTO, Integer statusId) {

		CustomerDecisionDTO customerDecisionDTO = new CustomerDecisionDTO(
				DateUtil.setCurrentTimeToDate(loanDTO.getContactDateCustomerDecision() != null
						? loanDTO.getContactDateCustomerDecision()
						: new Date()),
				loanDTO.getCommentsCustomerDecision(), loanDTO.getLoanId(), statusId,
				loanDTO.getApprovelAmount());
		CustomerDecisionDTO newCustomerDecisionDTO =
				customerDecisionService.save(customerDecisionDTO);
		logger.info("Customer Decision with status : {} was successfully inserted for loan : {}",
				newCustomerDecisionDTO.getStatusLibelle(), loanDTO.getAccountNumber());
	}

	/**
	 * Action upload signed agreements.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionUploadSignedAgreements(com.acm.utils.dtos
	 * .LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_UPLOAD_SIGNED_AGREEMENTS)
	public LoanDTO actionUploadSignedAgreements(LoanDTO loanDTO)
			throws ResourcesNotFoundException, UploadSignedDocNotFoundExepction,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		switch (loanDTO.getWorkflowNextAction()) {
			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE:
				if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
					actionUploadSignedAgreementsGroupe(loanDTO);
					// update next action
					loanDTO.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.ACTION_APPROVE_UPLOAD_SIGNED_AGREEMENTS);
					// Processing Loan in ACTIVITI
					// loanDTO = activitiProcessService.process(loanDTO);
					actionCentralRevision(loanDTO);
				}
				else {
					logger.info("Begin actionUploadSignedAgreements method...");
					// load list of document by loan
					AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
					acmDocumentsDTO.setLoanId(loanDTO.getLoanId());
					acmDocumentsDTO
							.setSettingDocumentTypeDTO(new SettingDocumentTypeDTO(CommonFunctions
									.mappingStatus(
											CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
									.getKey()));
					List<AcmDocumentsDTO> acmDocumentsDTOs =
							acmDocumentsService.find(acmDocumentsDTO);

					// check list documents inserted in ACM --> if exist in GED by
					// ID_DOCUMENT_GED
					Boolean documentsInsertedExistInGED = Boolean.TRUE;
					for (AcmDocumentsDTO documents : acmDocumentsDTOs) {
						if (Boolean.FALSE.equals(gedClient.isExist(new AcmDocumentsDTO(
								documents.getIdDocument(), documents.getIdDocumentGED())))) {
							documentsInsertedExistInGED = Boolean.FALSE;
						}
					}
					if (Boolean.TRUE.equals(documentsInsertedExistInGED)) {

						// update next action
						loanDTO.setWorkflowNextAction(
								ACMConstantWorkflowStatuts.ACTION_APPROVE_UPLOAD_SIGNED_AGREEMENTS);
						if (loanDTO.getEtapeWorkflow() <= 23) {
							// update statutWorkflow
							loanDTO.setStatutWorkflow(CommonFunctions
									.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION)
									.getKey());
							loanDTO.setStatutLibelle(CommonFunctions
									.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION)
									.getValue());
							// update EtapeWorkflow
							loanDTO.setEtapeWorkflow(CommonFunctions
									.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION)
									.getKey());
							// update status tab
							loanDTO.setStatut(CommonFunctions
									.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED)
									.getKey());

							// set the loan group owner and group owner name to CENTRAL_REVISION
							setLoanToGroupOfUsers(CommonConstants.CENTRAL_REVISION, loanDTO);
						}

						// setting date change status
						loanDTO.setChangeDateStatusWorkflow(new Date());
						loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
						// notif description
						String actionDescription =
								"-- UPLOAD SIGNED AGREEMENT -- Step has been validated at "
										+ DateUtil.formatDate(new Date(),
												CommonConstants.PATTREN_DATE)
										+ " By " + loanDTO.getOwnerName();

						// update loan in ABACUS
						try {
							if (loanDTO.getUpdateLoanAbacus()) {
								// update to abacus via API
								transversClient.updateLoanINDIVORG(loanDTO);
							}
						}
						catch (Exception e) {
							logger.error("Failed to add loan {}", e.getMessage());
							logger.error(
									CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
									e.getMessage());
							// INIT error message
							String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
							if (e != null && e.getMessage() != null
									&& e.getMessage().contains("errorMessage")) {
								String msgFromTransversApi =
										e.getMessage().substring(e.getMessage().indexOf('{'));
								final JsonNode jsonNode =
										new ObjectMapper().readTree(msgFromTransversApi);
								messageError = jsonNode.get("errorMessage").asText();
							}
							// Fire Exception
							throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
						}
						// update loan data
						loanService.save(loanDTO.getLoanId(), loanDTO);

						// Processing Loan in ACTIVITI
						if (loanDTO.getEtapeWorkflow() <= 23) {
							// loanDTO = activitiProcessService.process(loanDTO);
							actionCentralRevision(loanDTO);
							// NOTIFICATION
							if (loanDTO.getAssignedToOneUser()) {
								// notify the user
								notifyOneUserPerGroup(loanDTO, CommonConstants.CENTRAL_REVISION,
										actionDescription);
							}
							else {
								notifyUsersPerGroup(loanDTO, CommonConstants.CENTRAL_REVISION,
										actionDescription);
							}
						}
					}
					else {
						logger.error("Failed to documentsInsertedExistInGED !!!");
						logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
								Loan.class.getSimpleName());
						throw new UploadSignedDocNotFoundExepction(
								CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
								environment.getProperty(
										"workflow.exception.message.not.found.actionUploadSignedAgreements")
										+ Loan.class.getSimpleName()
										+ CommonExceptionsMessage.WITH_ID
										+ loanDTO.getAccountNumber());
					}

				}
				break;

			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW:
				// process review step
				loanDTO = actionCorrectifs(loanDTO);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			default:
				break;
		}

		logger.info("Complete to execute actionUploadSignedAgreements !!!");
		return loanDTO;
	}

	/**
	 * Action central revision.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionCentralRevision(com.acm.utils.dtos
	 * .LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CENTRAL_REVISION)
	public LoanDTO actionCentralRevision(LoanDTO loanDTO)
			throws ResourcesNotFoundException, UploadSignedDocNotFoundExepction,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		switch (loanDTO.getWorkflowNextAction()) {
			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE:
				if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
					actionCentralRevisionGroup(loanDTO);
					// update next action
					loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_YES);
					// Processing Loan in ACTIVITI
					// loanDTO = activitiProcessService.process(loanDTO);
					loanWorkflowSystemService.processEtatValide(loanDTO);
				}
				else {
					logger.info("Begin actionCentralRevision method...");
					// load list of document by loan
					AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
					acmDocumentsDTO.setLoanId(loanDTO.getLoanId());
					acmDocumentsDTO
							.setSettingDocumentTypeDTO(new SettingDocumentTypeDTO(CommonFunctions
									.mappingStatus(
											CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
									.getKey()));
					List<AcmDocumentsDTO> acmDocumentsDTOs =
							acmDocumentsService.find(acmDocumentsDTO);
					if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTOs)) {
						// check list documents inserted in ACM --> if exist in GED by
						// ID_DOCUMENT_GED
						Boolean documentsInsertedExistInGED = Boolean.TRUE;
						for (AcmDocumentsDTO documents : acmDocumentsDTOs) {
							if (Boolean.FALSE.equals(gedClient.isExist(new AcmDocumentsDTO(
									documents.getIdDocument(), documents.getIdDocumentGED())))) {
								documentsInsertedExistInGED = Boolean.FALSE;
							}
						}
						if (Boolean.TRUE.equals(documentsInsertedExistInGED)) {
							// ### update application data && lanch APPROVAL process = 2 ###
							if (loanDTO.getParentId() == 0) {
								// find product object by ID
								loanDTO.setProductDTO(parametrageClient
										.findProductById(loanDTO.getProductId().longValue()));
								// find UDF for loan
								UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
								params.setLoanId(loanDTO.getLoanId());
								loanDTO.setUserDefinedFieldsLinksDTOs(
										userDefinedFieldsLinksService.find(params));
								try {
									// update && call Approval API
									updateAndApprovalLoan(loanDTO, 2);
								}
								catch (Exception e) {
									logger.error(
											"Failed to execute method updateAndApprovalLoan in actionCentralRevision() : {}",
											e.getMessage());
									// INIT error message
									String messageError =
											"{\"errorMessage\":\" Error API Abacus\"}";
									if (e != null && e.getMessage() != null
											&& e.getMessage().contains("errorMessage")) {
										String msgFromTransversApi = e.getMessage()
												.substring(e.getMessage().indexOf('{'));
										final JsonNode jsonNode =
												new ObjectMapper().readTree(msgFromTransversApi);
										messageError = jsonNode.get("errorMessage").asText();
									}
									// Fire Exception
									throw new ApiAbacusException(CommonErrorCode.API_ABACUS,
											messageError);
								}
							}

							// update next action
							loanDTO.setWorkflowNextAction(
									ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_YES);
							// update loan data
							loanService.save(loanDTO.getLoanId(), loanDTO);
							// find list of accepted request of renewal condition by customer
							// list of accepted request for the customer
							AcmEnvironnementDTO renewalConditionSetting =
									parametrageClient.find("RENEWEL_LOAN_CONDITION");
							if (!ACMValidationUtils.isNullOrEmpty(renewalConditionSetting)
									&& renewalConditionSetting.getValue().equals("1")) {
								List<ExceptionRequestDTO> exceptionRequestDTOs =
										exceptionRequestService.find(new ExceptionRequestDTO(
												loanDTO.getCustomerDTO().getId(),
												CommonFunctions.mappingStatus(
														ACMConstantWorkflowStatuts.ACCEPTED_STATUT_REQUEST)
														.getKey()));
								// check if not null
								if (!ACMValidationUtils.isNullOrEmpty(exceptionRequestDTOs)) {
									// set statut close
									for (ExceptionRequestDTO exceptionRequest : exceptionRequestDTOs) {
										exceptionRequest.setStatut(CommonFunctions.mappingStatus(
												ACMConstantWorkflowStatuts.CLOSED_STATUT_REQUEST)
												.getKey());
										exceptionRequestService.save(exceptionRequest.getId(),
												exceptionRequest);
									}
								}
							}
							loanWorkflowSystemService.processEtatValide(loanDTO);
							// Processing Loan in ACTIVITI
							// loanDTO = activitiProcessService.process(loanDTO);
						}
						else {
							logger.error("Failed to documentsInsertedExistInGED !!!");
							logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
									Loan.class.getSimpleName());
							throw new UploadSignedDocNotFoundExepction(
									CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
									environment.getProperty(
											"workflow.exception.message.not.found.actionCentralRevision")
											+ Loan.class.getSimpleName()
											+ CommonExceptionsMessage.WITH_ID
											+ loanDTO.getAccountNumber());
						}
					}
					else {
						logger.error("Failed to execute actionCentralRevision !!!");
						logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
								Loan.class.getSimpleName());
						throw new UploadSignedDocNotFoundExepction(
								CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
								environment.getProperty(
										"workflow.exception.message.not.found.actionCentralRevision")
										+ Loan.class.getSimpleName()
										+ CommonExceptionsMessage.WITH_ID
										+ loanDTO.getAccountNumber());
					}
				}
				break;

			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW:
				// process review step
				loanDTO = actionCorrectifs(loanDTO);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				break;

			// workflow action back_by_step
			case ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW_AGREEMENT:
				// process back to step upload signed agreemnt
				loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_BACK_BY_STEP);
				// action in workflow schema
				loanDTO.setEtapeWorkflow(CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
						.getKey());
				// update status loan
				if (loanDTO.getEtapeWorkflow() <= 23) {
					loanDTO.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
							.getKey());
					loanDTO.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.UPLOAD_SIGNED_AGREEMENT)
							.getValue());
					// update status tab
					loanDTO.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED)
							.getKey());
				}
				loanDTO.setIhmRoot(parametrageClient
						.find(new HabilitationIHMRouteDTO(
								CommonConstants.ACM_IHM_UPLOAD_SIGNED_AGREEMENT))
						.get(0).getIhmRoute());
				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
				// notification description
				String actionDescription =
						"-- CENTRAL REVISION 2 -- Review Agreements Requested at "
								+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
								+ " By " + loanDTO.getOwnerName();
				// set group owner and owner name to BRANCHOPERATION GROUPE
				setLoanToGroupOfUsers(CommonConstants.BRANCH_OPERATION, loanDTO);
				// Processing Loan in ACTIVITI
				// loanDTO = activitiProcessService.process(loanDTO);
				actionUploadSignedAgreements(loanDTO);
				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);
				// save customer decision
				// set note
				loanDTO.setCommentsCustomerDecision(loanDTO.getNote());
				saveCustomerDecision(loanDTO,
						CommonFunctions.mappingStatus(
								ACMConstantWorkflowStatuts.CUSTOMER_DESICION_REVIEW_AGREEMENTS)
								.getKey());

				// Send notification to BRANCH OPERATION GROUP
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.BRANCH_OPERATION,
							actionDescription);
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.BRANCH_OPERATION,
							actionDescription);
				}
				break;

			default:
				break;
		}

		logger.info("Complete to execute actionCentralRevision !!!");
		return loanDTO;
	}

	/**
	 * Action correctifs.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionCorrectifs(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_CORRECTIFS)
	public LoanDTO actionCorrectifs(LoanDTO loanDTO) throws ResourcesNotFoundException {

		logger.info("Begin actionCorrectifs method...");
		loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_REVIEW);
		// action in workflow schema
		if (loanDTO.getEtapeWorkflow() < 23) {
			loanDTO.setEtapeWorkflow(
					CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
			loanDTO.setIhmRoot(
					parametrageClient.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME))
							.get(0).getIhmRoute());
		}
		// loanDTO.setStatutLibelle(
		// CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getValue());
		// update status loan
		loanDTO.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey());
		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_REVIEW).getKey());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// save historique approvel data
		if (loanDTO.getParentId() == 0) {
			saveLoanApprovalHistorique(loanDTO,
					CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
							.getKey());
		}

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		// update list participants
		loanParticipantsService
				.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

		if (loanDTO.getEtapeWorkflow() < 23) {
			// assign to default ACCOUNTPORTFOLIO_CODE
			UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
			loanDTO.setOwner(foundedUserDTO.getLogin());
			loanDTO.setOwnerName(foundedUserDTO.getSimpleName());

			// setting list participants
			LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
					.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
			logger.info("(actionCorrectifs) setting Loan Participants with ID = [{}] :: DONE",
					newLoanParticipantsDTO.getId());
		}
		logger.info("Complete to execute actionCorrectifs !!!");
		return loanDTO;
	}

	/**
	 * Action screening.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionScreening(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_SCREENING)
	public LoanDTO actionScreening(LoanDTO loanDTO) throws ResourcesNotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		logger.info("Begin actionScreening method...");
		if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
			// load group childs
			List<LoanDTO> loansChilds = loanService.findByParentId(loanDTO.getLoanId());
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_SCREENING);
			if (!ACMValidationUtils.isNullOrEmpty(loansChilds)) {
				// setting WorkflowNextAction for all childs
				for (LoanDTO child : loansChilds) {
					child.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
				}
				// complete workflow for all childs
				loanService.completeWorkflowForChilds(loanDTO, loansChilds);
			}
		}
		else {
			loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_SCREENING);
			// TODO processing cas REJET automatic => check with PO
		}

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		LoanDTO newLoan = loanWorkflowSystemService.processCheckScreening(loanDTO);
		logger.info("Complete to execute actionScreening !!!");
		return newLoan;
	}

	/**
	 * Action audit.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionAudit(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_AUDIT)
	public LoanDTO actionAudit(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException {

		logger.info("Begin actionAudit method...");

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
				if (loanDTO.getParentId() == 0) {
					// init key
					Integer key = CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_RISK_AUDIT_STATUS_RECOMMEND)
							.getKey();
					// setting status note to review
					if (loanDTO.getWorkflowNextAction()
							.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
						key = CommonFunctions
								.mappingStatus(
										ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
								.getKey();
					}
					// save historique risk audit recommend
					loanApprovalHistoriqueService
							.saveAndSetApprovalLabel(new LoanApprovalHistoriqueDTO(loanDTO,
									loanDTO.getApprovelAmount().longValue(), key, loanDTO.getNote(),
									loanDTO.getStatutWorkflow()));
				}
				// loading config "RISK_AMOUNT_VALIDATION"
				AcmEnvironnementDTO environnementDTO =
						parametrageClient.find("RISK_AMOUNT_VALIDATION");
				logger.info("{}", environnementDTO);
				if (!ACMValidationUtils.isNullOrEmpty(environnementDTO)) {
					// getting configured risk amount
					BigDecimal riskAmount = new BigDecimal(environnementDTO.getValue());
					// check next step based on loan amount
					if (loanDTO.getApplyAmountTotal().compareTo(riskAmount) >= 0
							&& !loanDTO.getWorkflowNextAction().equals(
									ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {

						// update status to RISK step
						loanDTO.setStatutWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey());
						loanDTO.setEtapeWorkflow(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getKey());
						loanDTO.setStatutLibelle(CommonFunctions
								.mappingStatus(ACMConstantWorkflowStatuts.RISK).getValue());

						loanDTO.setIhmRoot(parametrageClient
								.find(new HabilitationIHMRouteDTO(CommonConstants.ACM_HOME)).get(0)
								.getIhmRoute());

						// update list participants
						loanParticipantsService.update(
								new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

						// init messsage notif
						String actionDescription = "-- AUDIT -- Step has been validated at "
								+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
								+ " By " + loanDTO.getOwnerName();
						// assign to RISK Profil
						setLoanToGroupOfUsers(CommonConstants.RISK_MANAGER, loanDTO);
						// setting date change status
						loanDTO.setChangeDateStatusWorkflow(new Date());
						loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

						// update loan data
						loanService.save(loanDTO.getLoanId(), loanDTO);

						// Send notification to RISK MANAGER GROUP
						if (loanDTO.getAssignedToOneUser()) {
							// notify the user
							notifyOneUserPerGroup(loanDTO, CommonConstants.RISK_MANAGER,
									actionDescription);
						}
						else {
							notifyUsersPerGroup(loanDTO, CommonConstants.RISK_MANAGER,
									actionDescription);
						}

					}
					else if (loanDTO.getApplyAmountTotal().compareTo(riskAmount) < 0
							|| loanDTO.getWorkflowNextAction().equals(
									ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
						// Send Notif Review LO
						// assign to default ACCOUNTPORTFOLIO_CODE
						UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
						NotificationsDTO notificationsDTO =
								notificationsServices.save(new NotificationsDTO(
										foundedUserDTO.getLogin(), NotificationCategory.LOAN.name(),
										NotificationType.INFO.name(), Boolean.TRUE,
										CommonConstants.ACM_NOTIFICATION_ACTION_REVEIW, "", loanDTO,
										null));
						logger.info("New Notification  [{}] has been inserted.", notificationsDTO);
						logger.info(
								"workflow with ProcessInstanceId = [{}] was passed to next step for given loan with id = [{}]",
								loanDTO.getProcessInstanceId(), loanDTO.getLoanId());
						LoanDTO newLoanDTO =
								loanWorkflowSystemService.processSubmitLoanApproval(loanDTO);
						return newLoanDTO;
					}
				}
			}
			else if (Boolean.TRUE.equals(audit)) {
				// update list participants
				loanParticipantsService
						.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

				// assign to default audit owner
				UserDTO foundedUser = getOwnerAuditOrRisk(loanDTO);
				loanDTO.setOwner(foundedUser.getLogin());
				loanDTO.setOwnerName(foundedUser.getSimpleName());
				// setting list participants
				LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
						.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
				logger.info(
						"(process Audit-Risk ) setting Loan Participants with ID = [{}] :: DONE",
						newLoanParticipantsDTO.getId());

				// setting date change status
				loanDTO.setChangeDateStatusWorkflow(new Date());
				loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

				// update loan data
				loanService.save(loanDTO.getLoanId(), loanDTO);

				// save historique risk audit recommend
				if (loanDTO.getParentId() == 0) {
					// init key
					Integer key = CommonFunctions
							.mappingStatus(
									ACMConstantWorkflowStatuts.LOAN_RISK_AUDIT_STATUS_RECOMMEND)
							.getKey();
					// setting status note to review
					if (loanDTO.getWorkflowNextAction()
							.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
						key = CommonFunctions
								.mappingStatus(
										ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
								.getKey();
						// Send Notif Review LO
						UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
						NotificationsDTO notificationsDTO =
								notificationsServices.save(new NotificationsDTO(
										foundedUserDTO.getLogin(), NotificationCategory.LOAN.name(),
										NotificationType.INFO.name(), Boolean.TRUE,
										CommonConstants.ACM_NOTIFICATION_ACTION_REVEIW, "", loanDTO,
										null));
						logger.info("New Audit Notification  [{}] has been inserted.",
								notificationsDTO);
					}
					loanApprovalHistoriqueService
							.saveAndSetApprovalLabel(new LoanApprovalHistoriqueDTO(loanDTO,
									loanDTO.getApprovelAmount().longValue(), key, loanDTO.getNote(),
									loanDTO.getStatutWorkflow()));
				}
				LoanDTO newLoanDTO = loanWorkflowSystemService.processSubmitLoanApproval(loanDTO);
				return newLoanDTO;
			}
		}
		else {
			// assign to default ACCOUNTPORTFOLIO_CODE
			UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
			loanDTO.setOwner(foundedUserDTO.getLogin());
			loanDTO.setOwnerName(foundedUserDTO.getSimpleName());

			// setting date change status
			loanDTO.setChangeDateStatusWorkflow(new Date());
			loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

			// update loan data
			loanService.save(loanDTO.getLoanId(), loanDTO);
		}

		logger.info("Complete to execute actionAudit !!!");
		return loanDTO;

	}

	/**
	 * Action dynamic audit risk.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionDynamicAuditRisk(com.acm.utils.dtos.
	 * LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.DYNAMIC_WF_ACTION)
	public LoanDTO actionDynamicAuditRisk(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException {

		logger.info("Begin actionRisk method...");

		if (loanDTO.getParentId() == 0) {
			// init key
			Integer key = CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.LOAN_RISK_AUDIT_STATUS_RECOMMEND)
					.getKey();
			// setting status note to review
			if (loanDTO.getWorkflowNextAction()
					.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
				key = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
						.getKey();

				// assign to default ACCOUNTPORTFOLIO_CODE
				UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(foundedUserDTO.getLogin(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_REVEIW, "",
								loanDTO, null));
				logger.info("New Notification  [{}] has been inserted.", notificationsDTO);

			}
			// save historique risk audit recommend
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, loanDTO.getNote(), loanDTO.getStatutWorkflow()));

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
		}

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update list participants
		loanParticipantsService
				.update(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));

		return loanDTO;
	}

	/**
	 * Action risk.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws EnableCriticalDataException the enable critical data exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.LoanWorkflowUserActionService#actionRisk(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_RISK)
	public LoanDTO actionRisk(LoanDTO loanDTO)
			throws ResourcesNotFoundException, EnableCriticalDataException, CreditException {

		logger.info("Begin actionRisk method...");
		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update loan data
		loanService.save(loanDTO.getLoanId(), loanDTO);

		// setting list participants
		LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
				.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
		// save historique risk audit recommend
		if (loanDTO.getParentId() == 0) {
			// init key
			Integer key = CommonFunctions
					.mappingStatus(ACMConstantWorkflowStatuts.LOAN_RISK_AUDIT_STATUS_RECOMMEND)
					.getKey();
			// setting status note to review
			if (loanDTO.getWorkflowNextAction()
					.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_REVIEW)) {
				key = CommonFunctions
						.mappingStatus(ACMConstantWorkflowStatuts.LOAN_APPROVAL_STATUS_REVIEW)
						.getKey();

				// Send Notif Review LO
				UserDTO foundedUserDTO = getUsernameByPortfolioId(loanDTO);
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(foundedUserDTO.getLogin(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_REVEIW, "",
								loanDTO, null));
				logger.info("New ACM_NOTIFICATION_ACTION_REVEIW  [{}] has been inserted.",
						notificationsDTO);
			}
			loanApprovalHistoriqueService.saveAndSetApprovalLabel(
					new LoanApprovalHistoriqueDTO(loanDTO, loanDTO.getApprovelAmount().longValue(),
							key, loanDTO.getNote(), loanDTO.getStatutWorkflow()));
		}
		logger.info("(actionRisk) setting Loan Participants with ID = [{}] :: DONE",
				newLoanParticipantsDTO.getId());

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		LoanDTO newLoanDTO = loanWorkflowSystemService.processSubmitLoanApproval(loanDTO);
		logger.info("ActionRisk method :: DONE");
		return newLoanDTO;
	}

	/**
	 * Action complet data.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionCompletData(com.acm.utils.dtos.LoanDTO)
	 */
	@Override
	public LoanDTO actionCompletData(LoanDTO loanDTO)
			throws ResourcesNotFoundException, CreditException {

		logger.info("Begin actionCompletData method...");
		// find existing loan from db to check data
		LoanDTO existingLoanDTO = loanService.find(loanDTO.getLoanId());
		// check if All loan data has been completed
		if (existingLoanDTO.getUpdateLoan() == null
				|| Boolean.FALSE.equals(existingLoanDTO.getUpdateLoan())) {
			throw new CreditException(
					new ExceptionResponseMessage(CommonErrorCode.COMPLETE_LOAN_DATA_EXCEPTION,
							CommonExceptionsMessage.COMPLETE_LOAN_DATA_EXCEPTION,
							new TechnicalException()),
					CommonExceptionsMessage.COMPLETE_LOAN_DATA_EXCEPTION);
		}
		// check if All customer data has been completed
		if (existingLoanDTO.getCustomerDTO().getUpdateCustomer() == null
				|| Boolean.FALSE.equals(existingLoanDTO.getCustomerDTO().getUpdateCustomer())) {
			throw new CreditException(
					new ExceptionResponseMessage(CommonErrorCode.COMPLETE_CUSTOMER_DATA_EXCEPTION,
							CommonExceptionsMessage.COMPLETE_CUSTOMER_DATA_EXCEPTION,
							new TechnicalException()),
					CommonExceptionsMessage.COMPLETE_CUSTOMER_DATA_EXCEPTION);
		}

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_DRAFTS).getKey());
		// update loan data
		LoanDTO newLoanDTO = loanService.save(loanDTO.getLoanId(), loanDTO);

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);
		// check guarantor collateral setting
		LoanDTO newLoanDTOProcess =
				loanWorkflowSystemService.processCheckGuarantorCollateral(newLoanDTO);
		logger.info("Complete to execute actionCompletData !!!");
		return newLoanDTOProcess;
	}

	/**
	 * Action Upload Signed AgreementsGroupe.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the loanDTO
	 * @throws UploadSignedDocNotFoundExepction the Upload Signed Doc Not Found Exception
	 * @throws ResourcesNotFoundException the Resources Not Found Exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	private LoanDTO actionUploadSignedAgreementsGroupe(LoanDTO loanDTO)
			throws UploadSignedDocNotFoundExepction, ResourcesNotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		Boolean documentsInsertedExistInGED = Boolean.FALSE;
		Boolean resultatCheck = Boolean.FALSE;

		logger.info("Begin actionUploadSignedAgreements method...");
		// load list of document by loan
		List<LoanDTO> loansChilds = loanService.findByParentId(loanDTO.getLoanId());
		if (!ACMValidationUtils.isNullOrEmpty(loansChilds)) {
			for (LoanDTO loanDTOChild : loansChilds) {
				AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
				acmDocumentsDTO.setLoanId(loanDTOChild.getLoanId());
				acmDocumentsDTO.setSettingDocumentTypeDTO(new SettingDocumentTypeDTO(CommonFunctions
						.mappingStatus(CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
						.getKey()));
				List<AcmDocumentsDTO> acmDocumentsDTOs = acmDocumentsService.find(acmDocumentsDTO);
				loanDTOChild.setChildMissingInfo(Boolean.FALSE);
				// check Required Document
				loanDTOChild.setChildMissingInfo(
						acmDocumentsService.checkRequiredDocumentSigned(loanDTOChild));
				if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTOs)) {
					// check list documents inserted in ACM --> if exist in GED by ID_DOCUMENT_GED
					for (AcmDocumentsDTO documents : acmDocumentsDTOs) {
						if (Boolean.FALSE.equals(gedClient.isExist(new AcmDocumentsDTO(
								documents.getIdDocument(), documents.getIdDocumentGED())))) {
							documentsInsertedExistInGED = Boolean.TRUE;
							loanDTOChild.setChildMissingInfo(true);
						}
					}
				}
				loanDTOChild.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE);
			}
			List<Boolean> missingDoc = new ArrayList<>();
			loansChilds.stream()
					.forEach(loanChild -> missingDoc.add(loanChild.getChildMissingInfo()));
			if (missingDoc.contains(Boolean.FALSE)) {
				resultatCheck = Boolean.TRUE;
			}
			logger.info("Complete to execute actionUploadSignedAgreementsGroupe !!!");
			if (Boolean.FALSE.equals(resultatCheck)) {
				if (Boolean.TRUE.equals(!documentsInsertedExistInGED)) {

					// complete workflow for all childs
					loanService.completeWorkflowForChilds(loanDTO, loansChilds);

					// update next action
					loanDTO.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.ACTION_APPROVE_UPLOAD_SIGNED_AGREEMENTS);
					// update statutWorkflow
					loanDTO.setStatutWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION).getKey());
					loanDTO.setStatutLibelle(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION).getValue());
					// update EtapeWorkflow
					loanDTO.setEtapeWorkflow(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.CENTRAL_REVISION).getKey());
					// update status tab
					loanDTO.setStatut(CommonFunctions
							.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_APPROVED)
							.getKey());

					// setting date change status
					loanDTO.setChangeDateStatusWorkflow(new Date());
					loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

					// update list participants
					LoanParticipantsDTO loanParticipantsDTO =
							new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner());
					loanParticipantsService.update(loanParticipantsDTO);
					// init notification message
					String actionDescription =
							"-- UPLOAD SIGNED AGREEMENT -- Step has been validated at "
									+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
									+ " By " + loanDTO.getOwnerName();
					// set the loan group owner and group owner name to CENTRAL_REVISION
					setLoanToGroupOfUsers(CommonConstants.CENTRAL_REVISION, loanDTO);

					// setting list participants
					LoanParticipantsDTO newLoanParticipantsDTO = loanParticipantsService
							.save(new LoanParticipantsDTO(loanDTO.getLoanId(), loanDTO.getOwner()));
					logger.info(
							"(actionUploadSignedAgreements) setting Loan Participants with ID = [{}] :: DONE",
							newLoanParticipantsDTO.getId());
					// update loan in ABACUS
					try {
						if (loanDTO.getUpdateLoanAbacus()) {
							// update to abacus via API
							transversClient.updateLoanINDIVORG(loanDTO);
						}
					}
					catch (Exception e) {
						logger.error("Failed to add loan {}", e.getMessage());
						logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
								e.getMessage());
						// INIT error message
						String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
						if (e != null && e.getMessage() != null
								&& e.getMessage().contains("errorMessage")) {
							String msgFromTransversApi =
									e.getMessage().substring(e.getMessage().indexOf('{'));
							final JsonNode jsonNode =
									new ObjectMapper().readTree(msgFromTransversApi);
							messageError = jsonNode.get("errorMessage").asText();
						}
						// Fire Exception
						throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
					}
					loanService.save(loanDTO.getLoanId(), loanDTO);
					// send notification to the owner
					// Send notification to BRANCH OPERATION GROUP
					if (loanDTO.getAssignedToOneUser()) {
						// notify the user
						notifyOneUserPerGroup(loanDTO, CommonConstants.CENTRAL_REVISION,
								actionDescription);
					}
					else {
						notifyUsersPerGroup(loanDTO, CommonConstants.CENTRAL_REVISION,
								actionDescription);
					}

				}
				else {
					logger.error("Failed to execute actionUploadSignedAgreementsGroupe !!!");
					logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
					throw new UploadSignedDocNotFoundExepction(
							CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
							environment.getProperty(
									"workflow.exception.message.not.found.actionUploadSignedAgreements")
									+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
									+ loanDTO.getAccountNumber());
				}
			}
			else {
				logger.error("Failed to execute actionUploadSignedAgreementsGroupe !!!");
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new UploadSignedDocNotFoundExepction(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment.getProperty(
								"workflow.exception.message.not.found.actionUploadSignedAgreements")
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());
			}
		}
		return loanDTO;
	}

	/**
	 * Action central revision group.
	 *
	 * @author Ines Dridi
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws UploadSignedDocNotFoundExepction the upload signed doc not found exepction
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CheckApprovelLevelException the check approvel level exception
	 * @throws ApiAbacusException the api abacus exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws CreditException the credit exception
	 * @throws UploadDocumentNotFoundException the upload document not found exception
	 * @throws DisbursementException the disbursement exception
	 * @throws WorkFlowSettingException the work flow setting exception
	 * @throws CheckMezaCardException the check meza card exception
	 * @throws CheckMezaCardUntrustException the check meza card untrust exception
	 * @throws CheckFeesException the check fees exception
	 */
	private LoanDTO actionCentralRevisionGroup(LoanDTO loanDTO)
			throws UploadSignedDocNotFoundExepction, ResourcesNotFoundException,
			CheckApprovelLevelException, ApiAbacusException, IOException, CreditException,
			UploadDocumentNotFoundException, DisbursementException, WorkFlowSettingException,
			CheckMezaCardException, CheckMezaCardUntrustException, CheckFeesException {

		Boolean documentsInsertedExistInGED = Boolean.FALSE;
		Boolean resultatCheck = Boolean.FALSE;

		logger.info("Begin actionCentralRevisionGroup method...");
		// load list of document by loan
		List<LoanDTO> loansChilds = loanService.findByParentId(loanDTO.getLoanId());
		if (!ACMValidationUtils.isNullOrEmpty(loansChilds)) {
			for (LoanDTO loanDTOChild : loansChilds) {
				AcmDocumentsDTO acmDocumentsDTO = new AcmDocumentsDTO();
				acmDocumentsDTO.setLoanId(loanDTOChild.getLoanId());
				acmDocumentsDTO.setSettingDocumentTypeDTO(new SettingDocumentTypeDTO(CommonFunctions
						.mappingStatus(CommonConstants.ACM_SETTING_DOCS_ASSIGN_DOCUMENT_AUTRE)
						.getKey()));
				List<AcmDocumentsDTO> acmDocumentsDTOs = acmDocumentsService.find(acmDocumentsDTO);
				loanDTOChild.setChildMissingInfo(Boolean.FALSE);
				// check Required Document
				loanDTOChild.setChildMissingInfo(
						acmDocumentsService.checkRequiredDocumentSigned(loanDTOChild));
				if (!ACMValidationUtils.isNullOrEmpty(acmDocumentsDTOs)) {
					// check list documents inserted in ACM --> if exist in GED by ID_DOCUMENT_GED
					for (AcmDocumentsDTO documents : acmDocumentsDTOs) {
						if (Boolean.FALSE.equals(gedClient.isExist(new AcmDocumentsDTO(
								documents.getIdDocument(), documents.getIdDocumentGED())))) {
							documentsInsertedExistInGED = Boolean.TRUE;
							loanDTOChild.setChildMissingInfo(true);
						}
					}
				}
				loanDTOChild.setWorkflowNextAction(
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_APPROVE);
			}
			List<Boolean> missingDoc = new ArrayList<>();
			loansChilds.stream()
					.forEach(loanChild -> missingDoc.add(loanChild.getChildMissingInfo()));
			if (missingDoc.contains(Boolean.FALSE)) {
				resultatCheck = Boolean.TRUE;
			}
			logger.info("Complete to execute actionCentralRevisionGroup !!!");
			if (Boolean.FALSE.equals(resultatCheck)) {
				if (Boolean.TRUE.equals(!documentsInsertedExistInGED)) {

					// ### update application data && lanch APPROVAL process = 2 ###
					if (loanDTO.getParentId() == 0) {
						// find product object by ID
						loanDTO.setProductDTO(parametrageClient
								.findProductById(loanDTO.getProductId().longValue()));
						// find UDF for loan
						UserDefinedFieldsLinksDTO params = new UserDefinedFieldsLinksDTO();
						params.setLoanId(loanDTO.getLoanId());
						loanDTO.setUserDefinedFieldsLinksDTOs(
								userDefinedFieldsLinksService.find(params));
						try {
							// update && call Approval API
							updateAndApprovalLoan(loanDTO, 2);
						}
						catch (Exception e) {
							logger.error(
									"Failed to execute method updateAndApprovalLoan in actionCentralRevisionGroup() : {}",
									e.getMessage());
							// INIT error message
							String messageError = "{\"errorMessage\":\" Error API Abacus\"}";
							if (e != null && e.getMessage() != null
									&& e.getMessage().contains("errorMessage")) {
								String msgFromTransversApi =
										e.getMessage().substring(e.getMessage().indexOf('{'));
								final JsonNode jsonNode =
										new ObjectMapper().readTree(msgFromTransversApi);
								messageError = jsonNode.get("errorMessage").asText();
							}
							// Fire Exception
							throw new ApiAbacusException(CommonErrorCode.API_ABACUS, messageError);
						}
					}

					// complete workflow for all childs
					loanService.completeWorkflowForChilds(loanDTO, loansChilds);

					// update next action
					loanDTO.setWorkflowNextAction(ACMConstantWorkflowStatuts.ACTION_CHECK_DOC_YES);
					// update data
					loanService.save(loanDTO.getLoanId(), loanDTO);
				}
				else {
					logger.error("Failed to execute actionCentralRevisionGroup !!!");
					logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
					throw new UploadSignedDocNotFoundExepction(
							CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
							environment.getProperty(
									"workflow.exception.message.not.found.actionCentralRevisionGroup")
									+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
									+ loanDTO.getAccountNumber());
				}
			}
			else {
				logger.error("Failed to execute actionCentralRevisionGroup !!!");
				logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Loan.class.getSimpleName());
				throw new UploadSignedDocNotFoundExepction(
						CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
						environment.getProperty(
								"workflow.exception.message.not.found.actionCentralRevisionGroup")
								+ Loan.class.getSimpleName() + CommonExceptionsMessage.WITH_ID
								+ loanDTO.getAccountNumber());
			}
		}
		return loanDTO;
	}

	/**
	 * Adds the guarantor.
	 * 
	 * @author YesserSomai
	 * @param loanDTO the loan DTO
	 */
	private void addGuarantor(LoanDTO loanDTO) {

		logger.info("Begin addGuarantor method method...");
		// delete relation by id loan and Category
		customerLinksRelationshipService.deleteByIdLoanAndCategory(loanDTO.getLoanId(),
				CommonConstants.RELATION_GUARANTOR);
		// Add All relation
		for (CustomerDTO guarantor : loanDTO.getGuarantors()) {
			CustomerLinksRelationshipDTO customerLinksRelationshipDTO =
					new CustomerLinksRelationshipDTO(null, null, guarantor, null,
							CommonConstants.RELATION_GUARANTOR, new Date(), null,
							loanDTO.getLoanId(), guarantor.getAmountGuarantor());
			CustomerLinksRelationshipDTO newCustomerLinksRelationshipDTO =
					customerLinksRelationshipService.save(customerLinksRelationshipDTO);
			logger.info(" Links Relationship ADD LINKSRELATIONSHIP_ID = [{}]",
					newCustomerLinksRelationshipDTO.getId());
		}
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
	 * Action inform customer after docs signe.
	 *
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws InformCustomerNotFoundException the inform customer not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.service.LoanWorkflowUserActionService#actionInformCustomerAfterDocsSigne(com.acm.
	 * utils.dtos.LoanDTO)
	 */
	@Override
	@ProcessHistoryLoan(action = CommonAOPConstants.ACTION_ISSUED)
	public LoanDTO actionInformCustomerAfterDocsSigne(LoanDTO loanDTO)
			throws ResourcesNotFoundException, InformCustomerNotFoundException {

		logger.info("Begin actionInformCustomerAfterDocsSigne (DisbursementCaseClosure) method...");
		// update statutWorkflow
		loanDTO.setStatutWorkflow(
				CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey());
		// update status tab
		loanDTO.setStatut(CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_ISSUED).getKey());

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);
		loanService.save(loanDTO.getLoanId(), loanDTO);

		// UPDATE CHILD STATUS
		// if Loan is GRP : setting Loan Data for group child
		if (loanDTO.getCustomerType().equalsIgnoreCase(CustomerType.GRP.name())) {
			// load child
			List<LoanDTO> loansChilds = loanService.findByParentId(loanDTO.getLoanId());
			// update data for child
			for (LoanDTO child : loansChilds) {
				// setting owner
				child.setOwner(loanDTO.getOwner());
				child.setOwnerName(loanDTO.getOwnerName());
				settingLoanData(child, ACMConstantWorkflowStatuts.ACTION_APPROVED,
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getValue(),
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.ISSUED).getKey(),
						"",
						CommonFunctions.mappingStatus(ACMConstantWorkflowStatuts.STATUS_TAB_ISSUED)
								.getKey());
			}
		}

		// Processing Loan in ACTIVITI
		// loanDTO = activitiProcessService.process(loanDTO);

		logger.info("Complete to execute actionInformCustomerAfterDocsSigne !!!");
		return loanDTO;
	}

	/**
	 * Gets the default owner audit or risk.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the owner audit
	 */
	private UserDTO getOwnerAuditOrRisk(LoanDTO loanDTO) {

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
	 * Setting loan data.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param workflowNextAction the workflow next action
	 * @param statutWorkflow the statut workflow
	 * @param statutLibelle the statut libelle
	 * @param etapeWorkflow the etape workflow
	 * @param ihmRoot the ihm root
	 * @param statut the statut
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private LoanDTO settingLoanData(LoanDTO loanDTO, String workflowNextAction,
			Integer statutWorkflow, String statutLibelle, Integer etapeWorkflow, String ihmRoot,
			Integer statut) throws ResourcesNotFoundException {

		// setting data for loan
		loanDTO.setWorkflowNextAction(workflowNextAction);
		loanDTO.setStatutWorkflow(statutWorkflow);
		loanDTO.setStatutLibelle(statutLibelle);
		loanDTO.setEtapeWorkflow(etapeWorkflow);
		loanDTO.setIhmRoot(ihmRoot);

		// setting date change status
		loanDTO.setChangeDateStatusWorkflow(new Date());
		loanDTO.setCategory(CommonConstants.CATEGORY_INSTANCE);

		// update status tab if exist
		if (!ACMValidationUtils.isNullOrEmpty(statut) && statut != 0) {
			loanDTO.setStatut(statut);
		}
		// update loan data
		loanService.save(loanDTO.getLoanId(), loanDTO);

		// returning data
		return loanDTO;
	}

	/**
	 * Send mail.
	 * 
	 * @author Salmen Fatnassi
	 * @param mailLoanDTO the loanMail DTO
	 */
	private void sendMail(MailLoanDTO mailLoanDTO) {

		try {
			mailSenderClient.sendEmail(mailLoanDTO);
			logger.info("Sending Email Notification :: DONE");
		}
		catch (FeignException e) {
			logger.error("Failed to send Mail");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
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

			NotificationsDTO notificationsDTO = new NotificationsDTO();
			// list of users
			for (UserDTO userDTO : userDTOs) {
				switch (groupeCode) {
					case CommonConstants.BRANCH_OPERATION:
						notificationsDTO =
								notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
										NotificationCategory.LOAN.name(),
										NotificationType.INFO.name(), Boolean.TRUE,
										CommonConstants.ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION,
										actionDescription, loanDTO, null));
						logger.info("New Notification  [{}] has been inserted.", notificationsDTO);
						break;
					case CommonConstants.BRANCH_AUDITOR:
						notificationsDTO = notificationsServices.save(new NotificationsDTO(
								userDTO.getLogin(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_AUDIT, "", loanDTO, null));
						logger.info("New Audit Notification [{}] has been inserted.",
								notificationsDTO);
						break;
					case CommonConstants.RISK_MANAGER:
						notificationsDTO = notificationsServices.save(new NotificationsDTO(
								userDTO.getLogin(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_RISK, "", loanDTO, null));
						logger.info("New Risk Notification [{}] has been inserted.",
								notificationsDTO);
						break;
					case CommonConstants.CENTRAL_REVISION:
						notificationsDTO = notificationsServices.save(new NotificationsDTO(
								userDTO.getLogin(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_UPLOAD_SIGNED_AGREEMENT,
								actionDescription, loanDTO, null));
						logger.info("New Notification  [{}] has been inserted.", notificationsDTO);
						break;
					default:
						break;
				}

			}
		}

		return loanDTO;
	}

	/**
	 * Notify one user per group.
	 *
	 * @author ManelLamloum
	 * @param loanDTO the loan DTO
	 * @param groupeCode the groupe code
	 * @param actionDescription the action description
	 */
	private void notifyOneUserPerGroup(LoanDTO loanDTO, String groupeCode,
			String actionDescription) {

		// SEND NOTIFICATION TO THE USER TO WHICH THE LOAN IS ASSIGNED
		// Send notif in step AUDIT
		NotificationsDTO notificationsDTO = new NotificationsDTO();
		switch (groupeCode) {
			case CommonConstants.BRANCH_OPERATION:
				notificationsDTO = notificationsServices.save(
						new NotificationsDTO(loanDTO.getOwner(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_CUSTOMER_DECISION,
								actionDescription, loanDTO, null));
				logger.info("New Notification for BRANCH_OPERATION  [{}] has been inserted.",
						notificationsDTO);
				break;
			case CommonConstants.BRANCH_AUDITOR:
				notificationsDTO = notificationsServices.save(
						new NotificationsDTO(loanDTO.getOwner(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_AUDIT, "", loanDTO, null));
				logger.info("New Audit Notification for BRANCH_AUDITOR [{}] has been inserted.",
						notificationsDTO);
				break;
			case CommonConstants.RISK_MANAGER:
				notificationsDTO = notificationsServices.save(
						new NotificationsDTO(loanDTO.getOwner(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_RISK, "", loanDTO, null));
				logger.info("New Risk Notification for RISK_MANAGER [{}] has been inserted.",
						notificationsDTO);
				break;
			case CommonConstants.CENTRAL_REVISION:
				notificationsDTO = notificationsServices.save(
						new NotificationsDTO(loanDTO.getOwner(), NotificationCategory.LOAN.name(),
								NotificationType.INFO.name(), Boolean.TRUE,
								CommonConstants.ACM_NOTIFICATION_ACTION_UPLOAD_SIGNED_AGREEMENT,
								actionDescription, loanDTO, null));
				logger.info("New Notification for CENTRAL_REVISION  [{}] has been inserted.",
						notificationsDTO);
				break;
			default:
				break;
		}
	}
}
