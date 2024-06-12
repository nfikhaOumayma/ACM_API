/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.aop.history;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.acm.client.ReportingClient;
import com.acm.client.UserClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonAOPConstants;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.LoanHistoriqueService;
import com.acm.service.LoanService;
import com.acm.service.NotificationsServices;
import com.acm.service.UserNotificationService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanHistoriqueDTO;
import com.acm.utils.dtos.MailDTO;
import com.acm.utils.dtos.MailLoanDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.ReportVisitDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UsersNotificationsDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.MailBuilderMethod;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.string.StringUtils;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link ProcessHistoryLoanAspectImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Aspect
@Configuration
public class ProcessHistoryLoanAspectImpl {

	/** The logger. */
	private Logger logger = LoggerFactory.getLogger(this.getClass());

	/** The user client. */
	@Autowired(required = false)
	private UserClient userClient;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The loan historique service. */
	@Autowired
	private LoanHistoriqueService loanHistoriqueService;

	/** The user notification service. */
	@Autowired
	private UserNotificationService userNotificationService;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The mail sender client. */
	@Autowired
	private ReportingClient mailSenderClient;

	/** The default ACM receiver mail. */
	@Autowired
	private String defaultACMReceiverMail;

	/** The Constant SUBJECT_MAIL. */
	private static final String SUBJECT_MAIL = "Your loan application : ";

	/**
	 * Methods annoatated with history loan.
	 */
	@Pointcut(value = "@annotation(com.acm.aop.history.ProcessHistoryLoan)")
	public void methodsAnnoatatedWithHistoryLoan() {

		// defines pointcut for methods annotated with @ProcessHistoryLoan
	}

	/**
	 * After execution.
	 *
	 * @param joinPoint the join point
	 * @param result the result
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@AfterReturning(value = "methodsAnnoatatedWithHistoryLoan()  && @annotation(action)",
			returning = "result")
	public void afterExecution(JoinPoint joinPoint, Object result, ProcessHistoryLoan action)
			throws ResourcesNotFoundException {

		logger.info("Init History Loan AOP for {} Method", joinPoint.getSignature());
		if (result != null) {
			// save history
			saveHistory(result, action);
		}
	}

	/**
	 * Save history by given ACTION.
	 * 
	 * @author HaythemBenizid
	 * @param result the result
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void saveHistory(Object result, ProcessHistoryLoan action)
			throws ResourcesNotFoundException {

		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// process history by action

		switch (action.action()) {
			// SAVE_LOAN
			case CommonAOPConstants.SAVE_LOAN:
				saveLoan((LoanDTO) result, connectedUser, CommonAOPConstants.SAVE_LOAN);
				break;

			// REJECT_LOAN
			case CommonAOPConstants.REJECT_LOAN:
				rejectLoan((LoanDTO) result, connectedUser, CommonAOPConstants.REJECT_LOAN);
				break;

			// CANCEL_LOAN
			case CommonAOPConstants.CANCEL_LOAN:
				cancelLoan((LoanDTO) result, connectedUser, CommonAOPConstants.CANCEL_LOAN);
				break;

			// DECLINE_LOAN
			case CommonAOPConstants.DECLINE_LOAN:
				declineLoan((LoanDTO) result, connectedUser, CommonAOPConstants.DECLINE_LOAN);
				break;

			// ACTION_CORRECTIFS
			case CommonAOPConstants.ACTION_CORRECTIFS:
				actionReview((LoanDTO) result, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
				break;
			// SAVE_REASSIGN_LOAN
			case CommonAOPConstants.REASSIGN_LOAN:
				reassignLoan((LoanDTO) result, connectedUser, CommonAOPConstants.REASSIGN_LOAN);
				break;

			// ACTION_REVERSED_LOAN
			case CommonAOPConstants.REVERSED_LOAN:
				actionReversedLoan((LoanDTO) result, connectedUser,
						CommonAOPConstants.REVERSED_LOAN);
				break;

			// PROCESS_DETECT_NEW_LOAN
			case CommonAOPConstants.PROCESS_DETECT_NEW_LOAN:
				processDetectNewLoan((LoanDTO) result, connectedUser,
						CommonAOPConstants.PROCESS_DETECT_NEW_LOAN);
				break;
				
				// SAVE_ISSUED
			case CommonAOPConstants.ACTION_ISSUED:
				actionIssued((LoanDTO) result, connectedUser,
						CommonAOPConstants.ACTION_ISSUED);
				break;	

			default:
				// Check if Dynamic WF Action base on Workflow step
				String actionKey = ((LoanDTO) result).getEtapeWorkflow() <= 23 ? action.action()
						: CommonAOPConstants.DYNAMIC_WF_ACTION;
				switch (actionKey) {
					// ACTION_INITIAL_CHECK
					case CommonAOPConstants.ACTION_INITIAL_CHECK:
						actionInitialCheck((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_INITIAL_CHECK);
						break;

					// ACTION_CHECK_COLLATERAL
					case CommonAOPConstants.ACTION_CHECK_COLLATERAL:
						actionCheckCollateral((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_COLLATERAL);
						break;

					// ACTION_CHECK_GUARANTOR
					case CommonAOPConstants.ACTION_CHECK_GUARANTOR:
						actionCheckGuarantor((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_GUARANTOR);
						break;

					// ACTION_FIELD_VISIT
					case CommonAOPConstants.ACTION_FIELD_VISIT:
						actionFieldVisit((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_FIELD_VISIT);
						break;

					// ACTION_INFORM_CUSTOMER
					case CommonAOPConstants.ACTION_INFORM_CUSTOMER:
						actionInformCustomer((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_INFORM_CUSTOMER);
						break;

					// ACTION_UPLOAD_DOCUMENTS
					case CommonAOPConstants.ACTION_UPLOAD_DOCUMENTS:
						actionUploadDocuments((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_UPLOAD_DOCUMENTS);
						break;
					// ACTION_SCREENING
					case CommonAOPConstants.ACTION_SCREENING:
						actionScreening((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_SCREENING);
						break;

					// ACTION_ADD_FINANCIAL_ANALYSIS
					case CommonAOPConstants.ACTION_ADD_FINANCIAL_ANALYSIS:
						actionAddFinancialAnalysis((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_ADD_FINANCIAL_ANALYSIS);
						break;

					// ACTION_CHECK_L1
					case CommonAOPConstants.ACTION_CHECK_L1:
						actionCheckL1((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_L1);
						break;

					// ACTION_CHECK_L2
					case CommonAOPConstants.ACTION_CHECK_L2:
						actionCheckL2((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_L2);
						break;

					// ACTION_CHECK_L3
					case CommonAOPConstants.ACTION_CHECK_L3:
						actionCheckL3((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_L3);
						break;

					// ACTION_CHECK_L4
					case CommonAOPConstants.ACTION_CHECK_L4:
						actionCheckL4((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CHECK_L4);
						break;

					// ACTION_UPLOAD_SIGNED_AGREEMENTS
					case CommonAOPConstants.ACTION_UPLOAD_SIGNED_AGREEMENTS:
						actionUploadSignedAgreements((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_UPLOAD_SIGNED_AGREEMENTS);
						break;

					// PROCESS_ETAT_VALIDE
					case CommonAOPConstants.PROCESS_ETAT_VALIDE:
						processEtatValide((LoanDTO) result, connectedUser,
								CommonAOPConstants.PROCESS_ETAT_VALIDE);
						break;

					// PROCESS_SUBMIT_LOAN_APPROVAL
					case CommonAOPConstants.PROCESS_SUBMIT_LOAN_APPROVAL:
						processSubmitLoanApproval((LoanDTO) result, connectedUser,
								CommonAOPConstants.PROCESS_SUBMIT_LOAN_APPROVAL);
						break;

					// SAVE_REPORT_VISIT
					case CommonAOPConstants.SAVE_REPORT_VISIT:
						saveReportVisit((ReportVisitDTO) result, connectedUser,
								CommonAOPConstants.SAVE_REPORT_VISIT);
						break;

					// SAVE_RISK
					case CommonAOPConstants.ACTION_RISK:
						actionRisk((LoanDTO) result, connectedUser, CommonAOPConstants.ACTION_RISK);
						break;
					// SAVE_AUDIT
					case CommonAOPConstants.ACTION_AUDIT:
						actionAudit((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_AUDIT);
						break;

					// ACTION_CENTRAL_REVISION
					case CommonAOPConstants.ACTION_CENTRAL_REVISION:
						actionCentralRevision((LoanDTO) result, connectedUser,
								CommonAOPConstants.ACTION_CENTRAL_REVISION);
						break;
					// ACTION_VALIDATE_READY_FOR_DISBURSEMENT
					case CommonAOPConstants.VALIDATE_READY_FOR_DISBURSEMENT:
						actionReadyForDisbursement((LoanDTO) result, connectedUser,
								CommonAOPConstants.VALIDATE_READY_FOR_DISBURSEMENT);
						break;

					// DYNAMIC WF STEP ACTION
					case CommonAOPConstants.DYNAMIC_WF_ACTION:
						actionDynamicWF((LoanDTO) result, connectedUser,
								CommonAOPConstants.DYNAMIC_WF_ACTION);
						break;
					default:
						break;
				}
				break;
		}
	}

	/**
	 * Action ready for disbursement.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionReadyForDisbursement(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		// description
		String actionDescription = "-- Ready For Disbursement -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionReadyForDisbursement LOAN :: DONE ");

	}

	/**
	 * Action reversed loan.
	 * 
	 * @author idridi
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionReversedLoan(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		// description
		String actionDescription = "-- Reverse Loan -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionReversedLoan LOAN :: DONE ");

	}

	/**
	 * Action dynamic WF.
	 *
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionDynamicWF(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		saveHistorique(loanDTO, connectedUser, action, loanDTO.getStatutLibelle(),
				loanDTO.getCategory(), Boolean.FALSE);
		logger.info("HISTORY Dynamic WF action LOAN :: DONE ");

	}

	/**
	 * Save historique.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @param actionDescription the action description
	 * @param category the category
	 * @param techniqueInformation the techniqueInformation
	 */
	private void saveHistorique(LoanDTO loanDTO, UserDTO connectedUser, String action,
			String actionDescription, String category, Boolean techniqueInformation) {

		LoanHistoriqueDTO loanHistoriqueDTO = new LoanHistoriqueDTO(action, actionDescription,
				connectedUser.getFullName(), loanDTO, category, techniqueInformation);

		LoanHistoriqueDTO newHistoryLoanDTO = loanHistoriqueService.save(loanHistoriqueDTO);
		logger.info("{} action history has been saved with ID = [{}] for loan with ID  = [{}] ",
				action, newHistoryLoanDTO.getIdLoanHistorique(),
				newHistoryLoanDTO.getLoanDTO().getLoanId());
	}

	/**
	 * Load settings user notification for connected user.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	private List<String> loadSettingsUserNotification() {

		List<UsersNotificationsDTO> usersNotificationsDTOs = userNotificationService.find();
		List<String> settingsUserNotification = new ArrayList<>();
		// filtering data to get only category = WORKFLOW
		usersNotificationsDTOs.stream()
				.filter(userNotification -> userNotification.getSettingNotificationDTO()
						.getCategory().equals(CommonConstants.WORKFLOW))
				.forEach(dto -> settingsUserNotification
						.add(dto.getSettingNotificationDTO().getTypeNotif()));
		return settingsUserNotification;
	}

	/**
	 * Load settings user notification for user in parameter.
	 *
	 * @author ManelLamloum
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	private List<String> loadSettingsUserNotification(UserDTO userDTO) {

		// init params
		UsersNotificationsDTO params = new UsersNotificationsDTO();
		params.setUserDTO(userDTO);
		// find UsersNotifications
		List<UsersNotificationsDTO> usersNotificationsDTOs = userNotificationService.find(params);
		List<String> settingsUserNotification = new ArrayList<>();
		// filtering data to get only category = WORKFLOW
		usersNotificationsDTOs.stream()
				.filter(userNotification -> userNotification.getSettingNotificationDTO()
						.getCategory().equals(CommonConstants.WORKFLOW))
				.forEach(dto -> settingsUserNotification
						.add(dto.getSettingNotificationDTO().getTypeNotif()));
		return settingsUserNotification;
	}

	/**
	 * Send mail.
	 * 
	 * @author AbdelkarimTurki
	 * @author HaythemBenizid
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
	 * Gets the username by portfolio id.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @return the username by portfolio id
	 */
	private String getUsernameByPortfolioId(LoanDTO loanDTO) {

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
				userName = userDTOs.get(0).getLogin();
			}
		}
		logger.info(" founded USER_NAME = [{}]", userName);
		return userName;
	}

	/**
	 * Gets the user by owner.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @return the user by owner
	 */
	private UserDTO getUserByOwner(LoanDTO loanDTO) {

		UserDTO userDTO = new UserDTO();
		if (loanDTO.getOwner() != null) {
			userDTO = userClient.findByLogin(loanDTO.getOwner());
		}
		if (ACMValidationUtils.isNullOrEmpty(userDTO)
				&& Boolean.FALSE.equals(StringUtils.mailIsValid(userDTO.getEmail()))) {
			// default acm receiver mail
			userDTO.setEmail(defaultACMReceiverMail);
		}
		return userDTO;
	}

	/**
	 * Save loan.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void saveLoan(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		if (loanDTO != null && loanDTO.getLoanId() != null) {
			String actionDescription = "Loan has been inserted at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();

			// save Historique
			saveHistorique(loanDTO, connectedUser, action, actionDescription,
					CommonConstants.CATEGORY_INSTANCE, Boolean.TRUE);
			logger.info("HISTORY SAVED LOAN :: DONE ");
			// create notifcation only for loan INDIV / GROUPE (parent loan) / ORG
			if (loanDTO.getParentId() == 0) {
				// Notification
				NotificationsDTO notificationsDTO = notificationsServices
						.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_ADD,
								actionDescription, loanDTO, null));
				logger.info("New Notification = {} has been inserted.", notificationsDTO);
			}
		}
	}

	/**
	 * Save Report Visit.
	 *
	 * @author MoezMhiri
	 * @param reportVisitDTO the report visit DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void saveReportVisit(ReportVisitDTO reportVisitDTO, UserDTO connectedUser,
			String action) {

		String actionDescription = "New report visit has been inserted at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName() + " and the planned visit was at " + DateUtil
						.formatDate(reportVisitDTO.getPlannedVisit(), CommonConstants.PATTREN_DATE);
		saveHistorique(new LoanDTO(reportVisitDTO.getIdLoan()), connectedUser, action,
				actionDescription, CommonConstants.CATEGORY_INSTANCE, Boolean.FALSE);
		logger.info("HISTORY SAVED REPORT VISIT :: DONE ");
	}

	/**
	 * Reject loan.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void rejectLoan(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		String actionDescription = "Loan has been rejected at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY REJECT LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification = loadSettingsUserNotification();

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
							NotificationCategory.LOAN.name(), NotificationType.WARN.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_REJECT,
							actionDescription, loanDTO, null));
			logger.info("New Rejected Notification = {} has been inserted.", notificationsDTO);
		}
		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			// init notifications List
			List<LoanDTO> notificationsList = new ArrayList<>();
			// check type loan
			if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				// find list loan childs
				notificationsList = loanService.findByParentId(loanDTO.getLoanId());
			}
			else {
				notificationsList.add(loanDTO);
			}

			// loop && send notification mail
			for (LoanDTO loan : notificationsList) {
				sendMail(new MailLoanDTO(loan, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						(!ACMValidationUtils.isNullOrEmpty(loan.getCustomerDTO().getEmail())
								&& Boolean.TRUE.equals(
										StringUtils.mailIsValid(loan.getCustomerDTO().getEmail())))
												? loan.getCustomerDTO().getEmail()
												: defaultACMReceiverMail,
						SUBJECT_MAIL + loan.getAccountNumber(), "Loan Rejected Client Email."),
						MailBuilderMethod.BUILD_CUSTOMER_REJECT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Decline loan.
	 * 
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void declineLoan(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		String actionDescription = "Loan has been declined at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY DECLINE LOAN :: DONE ");

		// Get setting user notification
		List<String> settingsUserNotification = loadSettingsUserNotification();

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
							NotificationCategory.LOAN.name(), NotificationType.WARN.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_DECLINE,
							actionDescription, loanDTO, null));
			logger.info("New Declined Notification [{}] has been inserted.", notificationsDTO);
		}

		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			// init notifications List
			List<LoanDTO> notificationsList = new ArrayList<>();
			// check type loan
			if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				// find list loan childs
				notificationsList = loanService.findByParentId(loanDTO.getLoanId());
			}
			else {
				notificationsList.add(loanDTO);
			}

			// loop && send notification mail
			for (LoanDTO loan : notificationsList) {
				sendMail(new MailLoanDTO(loan, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						(!ACMValidationUtils.isNullOrEmpty(loan.getCustomerDTO().getEmail())
								&& Boolean.TRUE.equals(
										StringUtils.mailIsValid(loan.getCustomerDTO().getEmail())))
												? loan.getCustomerDTO().getEmail()
												: defaultACMReceiverMail,
						SUBJECT_MAIL + loan.getAccountNumber(), "Loan Declined Client Email."),
						MailBuilderMethod.BUILD_CUSTOMER_DECLINE, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Cancel loan.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void cancelLoan(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		String actionDescription = "Loan has been cancelled at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY CANCEL LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification = loadSettingsUserNotification();

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
							NotificationCategory.LOAN.name(), NotificationType.WARN.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_CANCEL,
							actionDescription, loanDTO, null));
			logger.info("New Canceled Notification = {} has been inserted.", notificationsDTO);
		}
		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			// init notifications List
			List<LoanDTO> notificationsList = new ArrayList<>();
			// check type loan
			if (loanDTO.getCustomerType().equals(CustomerType.GRP.name())) {
				// find list loan childs
				notificationsList = loanService.findByParentId(loanDTO.getLoanId());
			}
			else {
				notificationsList.add(loanDTO);
			}

			// loop && send notification mail
			for (LoanDTO loan : notificationsList) {
				sendMail(new MailLoanDTO(loan, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						(!ACMValidationUtils.isNullOrEmpty(loan.getCustomerDTO().getEmail())
								&& Boolean.TRUE.equals(
										StringUtils.mailIsValid(loan.getCustomerDTO().getEmail())))
												? loan.getCustomerDTO().getEmail()
												: defaultACMReceiverMail,
						SUBJECT_MAIL + loan.getAccountNumber(), "Loan Canceled Client Email."),
						MailBuilderMethod.BUILD_CUSTOMER_DECLINE, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action initial check.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionInitialCheck(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- INITIAL CHECK -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionInitialCheck LOAN :: DONE ");
	}

	/**
	 * Action check guarantor.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionCheckGuarantor(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- CHECK GUARANTOR -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionCheckGuarantor LOAN :: DONE ");
	}

	/**
	 * Action check collateral.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionCheckCollateral(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- CHECK COLLATERAL -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionCheckCollateral LOAN :: DONE ");
	}

	/**
	 * Action Review.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionReview(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- REVIEW -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);

		logger.info("HISTORY actionReview LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification =
				loadSettingsUserNotification(getUserByOwner(loanDTO));

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
							NotificationCategory.LOAN.name(), NotificationType.WARN.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_REVEIW,
							actionDescription, loanDTO, null));
			logger.info("New Review Notification = {} has been inserted.", notificationsDTO);
		}

		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			sendMail(new MailLoanDTO(loanDTO,
					new MailDTO(CommonConstants.NO_REPLAY_EMAIL, getUserByOwner(loanDTO).getEmail(),
							SUBJECT_MAIL + loanDTO.getAccountNumber(),
							"Loan has been reassign to be reviewed."),
					MailBuilderMethod.BUILD_REVIEW, connectedUser.getFullName()));
		}
	}

	/**
	 * Action actionAskForReview.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionAskForReview(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- ASK FOR REVIEW -- Step reassign to be reviewed at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionAskForReview LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification =
				loadSettingsUserNotification(getUserByOwner(loanDTO));

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO = notificationsServices
					.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
							NotificationCategory.LOAN.name(), NotificationType.WARN.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_CLIENT_ASK_REVIEW,
							actionDescription, loanDTO, null));
			logger.info("New ask for Review Notification = {} has been inserted.",
					notificationsDTO);
		}

		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			sendMail(new MailLoanDTO(loanDTO,
					new MailDTO(CommonConstants.NO_REPLAY_EMAIL, getUserByOwner(loanDTO).getEmail(),
							SUBJECT_MAIL + loanDTO.getAccountNumber(),
							"Loan has been reassign to be reviewed."),
					MailBuilderMethod.BUILD_CUSTOMER_ASK_FOR_REVIEW, connectedUser.getFullName()));
		}
	}

	/**
	 * Action upload signed agreements.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionUploadSignedAgreements(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {
			String actionDescription = "-- UPLOAD SIGNED AGREEMENTS -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionUploadSignedAgreements LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification = loadSettingsUserNotification();

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO = notificationsServices
						.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_APPROVE,
								actionDescription, loanDTO, null));
				logger.info("New actionUploadSignedAgreements Notification = {} has been inserted.",
						notificationsDTO);
			}
		}
		else {
			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
	}

	/**
	 * Action central revision.
	 *
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionCentralRevision(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {
			String actionDescription = "";
			// if action is review agreements
			if (loanDTO.getWorkflowNextAction()
					.equals(ACMConstantWorkflowStatuts.ACTION_BACK_BY_STEP)) {
				actionDescription =
						"-- CENTRAL REVISION REVIEW AGREEMENTS -- Step has been validated at "
								+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
								+ " By " + connectedUser.getFullName();
				saveHistorique(loanDTO, connectedUser, CommonAOPConstants.ACTION_REVIEW_AGREEMENTS,
						actionDescription, loanDTO.getCategory(), Boolean.FALSE);
				logger.info("HISTORY actionReviewAgreements LOAN :: DONE ");
			}
			else {
				actionDescription = "-- CENTRAL REVISION -- Step has been validated at "
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ connectedUser.getFullName();
				saveHistorique(loanDTO, connectedUser, action, actionDescription,
						loanDTO.getCategory(), Boolean.FALSE);
				logger.info("HISTORY actionCentralRevision LOAN :: DONE ");
				// Get setting user notification
				List<String> settingsUserNotification = loadSettingsUserNotification();

				// Send Notif
				if (settingsUserNotification.contains(CommonConstants.NOTIF)
						&& loanDTO.getParentId() == 0) {
					NotificationsDTO notificationsDTO = notificationsServices
							.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE,
									CommonConstants.ACM_NOTIFICATION_READY_FOR_DISBURSMENT,
									actionDescription, loanDTO, null));
					logger.info("New actionCentralRevision Notification = {} has been inserted.",
							notificationsDTO);
				}

			}
		}
		else {
			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
	}

	/**
	 * Action check L 4.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionCheckL4(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {

			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
		else if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()) {
			String actionDescription = "-- APPROVEL LEVEL 4 -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionCheckL4 LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Approve L4 Notification = {} has been inserted.",
						notificationsDTO);
			}

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						getUserByOwner(loanDTO).getEmail(),
						SUBJECT_MAIL + loanDTO.getAccountNumber(), "Loan has been approved L4."),
						MailBuilderMethod.BUILD_SUBMIT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action check L 3.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionCheckL3(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {

			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
		else if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()) {
			String actionDescription = "-- APPROVEL LEVEL 3 -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionCheckL3 LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Approve L3 Notification = {} has been inserted.",
						notificationsDTO);
			}

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						getUserByOwner(loanDTO).getEmail(),
						SUBJECT_MAIL + loanDTO.getAccountNumber(), "Loan has been approved L3."),
						MailBuilderMethod.BUILD_SUBMIT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action check L 2.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionCheckL2(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {

			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
		else if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()) {
			String actionDescription = "-- APPROVEL LEVEL 2 -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionCheckL2 LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Approve L2 Notification [{}] has been inserted.",
						notificationsDTO);
			}

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						getUserByOwner(loanDTO).getEmail(),
						SUBJECT_MAIL + loanDTO.getAccountNumber(), "Loan has been approved L2."),
						MailBuilderMethod.BUILD_SUBMIT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action check L 1.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionCheckL1(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {

			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
		else if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REJECTED).getKey()) {
			String actionDescription = "-- APPROVEL LEVEL 1 -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionCheckL1 LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New Approve L1 Notification [{}] has been inserted.",
						notificationsDTO);
			}

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						getUserByOwner(loanDTO).getEmail(),
						SUBJECT_MAIL + loanDTO.getAccountNumber(), "Loan has been approved L1."),
						MailBuilderMethod.BUILD_SUBMIT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action add financial analysis.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionAddFinancialAnalysis(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		String actionDescription = "-- FINANCIAL ANALYSIS -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionAddFinancialAnalysis LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification = loadSettingsUserNotification();

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(connectedUser.getLogin(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
							actionDescription, loanDTO, null));
			logger.info("New actionAddFinancialAnalysis Notification [{}] has been inserted.",
					notificationsDTO);
		}
	}

	/**
	 * Action upload documents.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionUploadDocuments(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- UPLOAD DOCUMENTS -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionUploadDocuments LOAN :: DONE ");
	}

	/**
	 * Action screening.
	 *
	 * @author Ines Dridi
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionScreening(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- SCREENING -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionScreening LOAN :: DONE ");
	}

	/**
	 * Action inform customer.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionInformCustomer(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		if (loanDTO.getWorkflowNextAction().equals(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_NO)
				|| loanDTO.getWorkflowNextAction()
						.equals(ACMConstantWorkflowStatuts.ACTION_REVIEW)) {
			actionAskForReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
		if (loanDTO.getWorkflowNextAction()
				.equals(ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_DECLINED)) {
			declineLoan(loanDTO, connectedUser, CommonAOPConstants.DECLINE_LOAN);
		}
		else if (loanDTO.getWorkflowNextAction()
				.equals(ACMConstantWorkflowStatuts.ACTION_DATA_VALID_YES)
				|| loanDTO.getWorkflowNextAction()
						.equals(ACMConstantWorkflowStatuts.ACTION_CUSTOMER_AGREED_YES)) {
			String actionDescription = "-- INFORM CUSTOMER -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionInformCustomer LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO, new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
						getUserByOwner(loanDTO).getEmail(),
						SUBJECT_MAIL + loanDTO.getAccountNumber(), "Loan has been Accepted."),
						MailBuilderMethod.BUILD_CUSTOMER_ACCEPT, connectedUser.getFullName()));
			}
		}
	}

	/**
	 * Action field visit.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionFieldVisit(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "-- FIELD VISIT -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionFieldVisit LOAN :: DONE ");
	}

	/**
	 * Process detect new loan.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void processDetectNewLoan(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "Loan has been initialized in workflow at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription,
				CommonConstants.CATEGORY_INSTANCE, Boolean.FALSE);
		logger.info("HISTORY processDetectNewLoan LOAN :: DONE ");
	}

	/**
	 * Process etat valide.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void processEtatValide(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "Loan has been validate at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY processEtatValide LOAN :: DONE ");
	}

	/**
	 * process Submit Loan Approval.
	 *
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void processSubmitLoanApproval(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.APPROVAL_L1).getKey()) {
			String actionDescription = "Loan has been submited to be approved at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY processSubmitLoanApproval LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification =
					loadSettingsUserNotification(getUserByOwner(loanDTO));

			// Send Notif
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO =
						notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("send to approve Notification [{}] has been inserted.",
						notificationsDTO);
			}

			// Send Mail
			if (settingsUserNotification.contains(CommonConstants.EMAIL)
					&& loanDTO.getParentId() == 0) {
				sendMail(new MailLoanDTO(loanDTO,
						new MailDTO(CommonConstants.NO_REPLAY_EMAIL,
								getUserByOwner(loanDTO).getEmail(),
								SUBJECT_MAIL + loanDTO.getAccountNumber(),
								"Loan has been submited to be approved."),
						MailBuilderMethod.BUILD_SUBMIT, connectedUser.getFullName()));
			}
		}
		else if (loanDTO.getStatutWorkflow() == CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {
			actionReview(loanDTO, connectedUser, CommonAOPConstants.ACTION_CORRECTIFS);
		}
	}

	/**
	 * Reassign loan.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void reassignLoan(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		String actionDescription = "Loan has been re-assigned at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName() + " To " + loanDTO.getOwnerName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY reassignLoan LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification =
				loadSettingsUserNotification(getUserByOwner(loanDTO));

		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_REASSIGN,
							actionDescription, loanDTO, null));
			logger.info("New Reassign Notification [{}] has been inserted.", notificationsDTO);
		}

		// Send Mail
		if (settingsUserNotification.contains(CommonConstants.EMAIL)
				&& loanDTO.getParentId() == 0) {
			sendMail(new MailLoanDTO(loanDTO,
					new MailDTO(CommonConstants.NO_REPLAY_EMAIL, getUserByOwner(loanDTO).getEmail(),
							SUBJECT_MAIL + loanDTO.getAccountNumber(),
							"Loan Re-assign to " + loanDTO.getOwnerName()),
					MailBuilderMethod.BUILD_GENERIC, connectedUser.getFullName()));
		}
	}

	/**
	 * Action audit.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionAudit(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {
			String actionDescription = "-- AUDIT -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionAudit LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification = loadSettingsUserNotification();

			// Send Notif to Loan Portfolio User
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO = notificationsServices
						.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New actionAudit Notification = {} has been inserted.",
						notificationsDTO);
			}
		}
	}

	/**
	 * Action risk.
	 *
	 * @author MoezMhiri
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void actionRisk(LoanDTO loanDTO, UserDTO connectedUser, String action) {

		if (loanDTO.getStatutWorkflow() != CommonFunctions
				.mappingStatus(ACMConstantWorkflowStatuts.REVIEW).getKey()) {
			String actionDescription = "-- RISK -- Step has been validated at "
					+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
					+ connectedUser.getFullName();
			saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
					Boolean.FALSE);
			logger.info("HISTORY actionRisk LOAN :: DONE ");
			// Get setting user notification
			List<String> settingsUserNotification = loadSettingsUserNotification();

			// Send Notif to Loan Portfolio User
			if (settingsUserNotification.contains(CommonConstants.NOTIF)
					&& loanDTO.getParentId() == 0) {
				NotificationsDTO notificationsDTO = notificationsServices
						.save(new NotificationsDTO(getUsernameByPortfolioId(loanDTO),
								NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
								Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_SUBMITTED,
								actionDescription, loanDTO, null));
				logger.info("New actionRisk Notification = {} has been inserted.",
						notificationsDTO);
			}
		}
	}

	/**
	 * Action issued.
	 * 
	 * @author HaythemBenizid
	 * @param loanDTO the loan DTO
	 * @param connectedUser the connected user
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void actionIssued(LoanDTO loanDTO, UserDTO connectedUser, String action)
			throws ResourcesNotFoundException {

		String actionDescription = "-- ISSUED -- Step has been validated at "
				+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
				+ connectedUser.getFullName();
		saveHistorique(loanDTO, connectedUser, action, actionDescription, loanDTO.getCategory(),
				Boolean.FALSE);
		logger.info("HISTORY actionIssued LOAN :: DONE ");
		// Get setting user notification
		List<String> settingsUserNotification = loadSettingsUserNotification();
		// Send Notif
		if (settingsUserNotification.contains(CommonConstants.NOTIF)
				&& loanDTO.getParentId() == 0) {
			NotificationsDTO notificationsDTO =
					notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
							NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
							Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_ISSUED,
							actionDescription, loanDTO, null));
			logger.info("New action Issued Notification [{}] has been inserted.", notificationsDTO);
		}
	}
}
