/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.GedClient;
import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.type.AMLPourcentageConfigurationException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.IScoreExpiryDayException;
import com.acm.exceptions.type.IScoreExpiryDayFailedErrorException;
import com.acm.exceptions.type.IScoreProductConfigurationException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ThirdPartyMappingDataRepository;
import com.acm.service.CustomerService;
import com.acm.service.LoanParticipantsService;
import com.acm.service.LoanService;
import com.acm.service.NotificationsServices;
import com.acm.service.ThirdPartyHistoriqueService;
import com.acm.service.ThirdPartyService;
import com.acm.soap.kyc.model.KycDTO;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AMLDataDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.GedParameterDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanParticipantsDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.NotificationCategory;
import com.acm.utils.enums.NotificationType;
import com.acm.utils.enums.ThirdPartyCategory;
import com.acm.utils.enums.ThirdPartyStatus;
import com.acm.utils.models.ThirdPartyMappingData;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link ThirdPartyServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@Service
public class ThirdPartyServiceImpl implements ThirdPartyService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ThirdPartyServiceImpl.class);

	/** The third party historique service. */
	@Autowired
	private ThirdPartyHistoriqueService thirdPartyHistoriqueService;

	/** The loan service. */
	@Autowired
	private LoanService loanService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The loan participants service. */
	@Autowired
	private LoanParticipantsService loanParticipantsService;

	/** The notifications services. */
	@Autowired
	private NotificationsServices notificationsServices;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The userClient. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The Constant CUSTOMER. */
	private static final String CUSTOMER = "CUSTOMER";

	/** The Constant GUARANTOR. */
	private static final String GUARANTOR = "GUARANTOR";

	/** The ged client. */
	@Autowired
	private GedClient gedClient;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The third party transvers client. */
	@Autowired
	private TransversClient thirdPartyTransversClient;

	/** The third party mapping data repository. */
	@Autowired
	private ThirdPartyMappingDataRepository thirdPartyMappingDataRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyService#checkAML(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public ScreeningDTO checkAML(ScreeningDTO screeningDTO)
			throws AMLPourcentageConfigurationException {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// LOADING CONFIG "CHACK_AML_MIN_POURCENTAGE"
		AcmEnvironnementDTO environnementDTO = parametrageClient.find("CHACK_AML_MIN_POURCENTAGE");
		logger.info("{}", environnementDTO);
		if (ACMValidationUtils.isNullOrEmpty(environnementDTO)
				|| ACMValidationUtils.isNullOrEmpty(environnementDTO.getValue())) {
			logger.error(CommonExceptionsMessage.AML_POURCENTAGE_CONFIGURATION);
			throw new AMLPourcentageConfigurationException(
					new ExceptionResponseMessage(CommonErrorCode.AML_POURCENTAGE_CONFIGURATION,
							CommonExceptionsMessage.AML_POURCENTAGE_CONFIGURATION),
					CommonExceptionsMessage.AML_POURCENTAGE_CONFIGURATION);
		}
		try {
			// init Loan loan by ID
			screeningDTO.setLoanDTO(new LoanDTO(screeningDTO.getIdLoan()));
			// Init params && send AML check to parametrage-service
			AMLDataDTO paramsAMLDataDTO = new AMLDataDTO();
			// 1 STEP : find by NAME
			paramsAMLDataDTO
					.setName(screeningDTO.getCustomerDTO().getCustomerName().replace("|", " "));
			List<AMLDataDTO> amlDataDTOs = parametrageClient.findAMLData(paramsAMLDataDTO);
			Integer amlPourcentage = 0;
			// processing response
			ScreeningDTO responseScreeningDTO = screeningDTO;
			// mapping status AML
			String statusAML = ThirdPartyStatus.ERROR.name();
			if (ACMValidationUtils.isNullOrEmpty(amlDataDTOs)
					|| amlDataDTOs.get(0).getName().equals(CommonConstants.NO_MATCH)) {
				// setting status to ACCEPTED
				statusAML = ThirdPartyStatus.ACCEPTED.name();
				// check if the customer pays the full repayment on the first 4 months
				Map<Long, List<ScheduleDTO>> mapScheduleDTOs = transversClient
						.loadCheckPaiment(screeningDTO.getCustomerDTO().getCustomerNumber());
				// it should be assign to user for a check the actor called : الأموال غسل
				if (Boolean.TRUE.equals(checkFullRepaiment(mapScheduleDTOs))) {
					// Setting status to REVIEW_GUARANTOR
					if (!ACMValidationUtils.isNullOrEmpty(screeningDTO.getIdLoan())) {
						LoanDTO loanDTO = loanService.find(screeningDTO.getIdLoan());
						// Assign to COMPLIANCE_GROUP
						String actionDescription = "-- SCREENING -- AML checked at"
								+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE)
								+ " By " + loanDTO.getOwnerName();
						// assign loan to COMPLIANCE_GROUP
						setLoanToGroupOfUsers(CommonConstants.COMPLIANCE_GROUP, loanDTO);
						// Send notification to RISK MANAGER GROUP
						if (Boolean.TRUE.equals(loanDTO.getAssignedToOneUser())) {
							notifyOneUserPerGroup(loanDTO, CommonConstants.COMPLIANCE_GROUP,
									actionDescription);
						}
						else {
							notifyUsersPerGroup(loanDTO, CommonConstants.COMPLIANCE_GROUP,
									actionDescription);
						}
					}
					if (screeningDTO.getCustomerCategory().equals(CUSTOMER)) {
						statusAML = ThirdPartyStatus.REFERED.name();
					}
					else if (screeningDTO.getCustomerCategory().equals(GUARANTOR)) {
						statusAML = ThirdPartyStatus.REVIEW_GUARANTOR.name();
					}
					amlDataDTOs.add(new AMLDataDTO("MONEY LAUNDERING SUSPECT",
							"Money Laundering Suspect", "Money Laundering Suspect"));
				}
				logger.info("After Check AML : STATUS =  {}", statusAML);
				// saving AML history
				responseScreeningDTO.setThirdPartyHistoriqueDTO(
						saveThirdPartyHistorique(screeningDTO, ThirdPartyCategory.AML.name(),
								CommonFunctions.convertObjectToJSONString(paramsAMLDataDTO),
								CommonFunctions.convertObjectToJSONString(amlDataDTOs), statusAML,
								null, amlPourcentage));
			}
			else if (!ACMValidationUtils.isNullOrEmpty(amlDataDTOs)
					&& !(amlDataDTOs.get(0).getName().equals(ThirdPartyStatus.ERROR.name()))) {
				// setting status to REFERED
				statusAML = ThirdPartyStatus.REFERED.name();
				// setting score to : 100 => match founded
				amlPourcentage = 100;
				responseScreeningDTO.setThirdPartyHistoriqueDTO(
						saveThirdPartyHistorique(screeningDTO, ThirdPartyCategory.AML.name(),
								CommonFunctions.convertObjectToJSONString(paramsAMLDataDTO),
								CommonFunctions.convertObjectToJSONString(amlDataDTOs), statusAML,
								null, amlPourcentage));
				// assing loan to COMPLIANCE_GROUP
				if (!ACMValidationUtils.isNullOrEmpty(screeningDTO.getIdLoan())) {
					LoanDTO loanDTO = loanService.find(screeningDTO.getIdLoan());
					// Assign to COMPLIANCE_GROUP
					String actionDescription = "-- SCREENING -- AML checked at"
							+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
							+ loanDTO.getOwnerName();
					// assign loan to COMPLIANCE_GROUP
					setLoanToGroupOfUsers(CommonConstants.COMPLIANCE_GROUP, loanDTO);
					// Send notification to COMPLIANCE_GROUP
					if (Boolean.TRUE.equals(loanDTO.getAssignedToOneUser())) {
						// notify the user
						notifyOneUserPerGroup(loanDTO, CommonConstants.COMPLIANCE_GROUP,
								actionDescription);
					}
					else {
						notifyUsersPerGroup(loanDTO, CommonConstants.COMPLIANCE_GROUP,
								actionDescription);
					}

				}
			}
			else if (!ACMValidationUtils.isNullOrEmpty(amlDataDTOs)
					&& (amlDataDTOs.get(0).getName().equals(ThirdPartyStatus.ERROR.name()))) {
				responseScreeningDTO.setThirdPartyHistoriqueDTO(saveThirdPartyHistorique(
						screeningDTO, ThirdPartyCategory.AML.name(), "No Request !!",
						CommonFunctions.convertObjectToJSONString(amlDataDTOs),
						ThirdPartyStatus.ERROR.name(), null, amlPourcentage));
			}
			// setting status
			responseScreeningDTO.setDecision(statusAML);
			logger.info("status AML= {}", statusAML);
			// returning data
			return responseScreeningDTO;
		}
		catch (Exception e) {
			logger.error("Error will calling AML check method : {}", e.getMessage());
			screeningDTO.setThirdPartyHistoriqueDTO(saveThirdPartyHistorique(screeningDTO,
					ThirdPartyCategory.AML.name(), "No Request !!",
					CommonFunctions.convertObjectToJSONString(new ArrayList<>()),
					ThirdPartyStatus.ERROR.name(), null, 0));
			screeningDTO.setDecision(ThirdPartyStatus.ERROR.name());
			return screeningDTO;
		}
	}

	/**
	 * Check LOAN full repaiment : if TermPeriodNum > 4 and the loan is fully paid in 4 Month or
	 * less it will be assign to COMPLIANCE_GROUP.
	 *
	 * @author HaythemBenizid
	 * @param mapScheduleDTOs the map schedule DT os
	 * @return the boolean
	 */
	private Boolean checkFullRepaiment(Map<Long, List<ScheduleDTO>> mapScheduleDTOs) {

		Boolean result = Boolean.FALSE;
		try {
			if (!ACMValidationUtils.isNullOrEmpty(mapScheduleDTOs)) {
				for (Map.Entry<Long, List<ScheduleDTO>> entry : mapScheduleDTOs.entrySet()) {
					logger.info("loanId = {}", entry.getKey());
					// Check if the loan is Full paid
					Boolean isNotYetPaid = entry.getValue().stream()
							.anyMatch(schedule -> schedule.getPaidOn().equals("Not_yet_Paid"));
					// If LOAN is fully paid
					if (Boolean.FALSE.equals(isNotYetPaid)) {
						Date firstDate = new SimpleDateFormat(CommonConstants.PATTREN_DATE)
								.parse(entry.getValue().get(0).getPaidOn());
						logger.info("FIRST_DATE = {}", firstDate);
						Date endDate = new SimpleDateFormat(CommonConstants.PATTREN_DATE).parse(
								entry.getValue().get(entry.getValue().size() - 1).getPaidOn());
						logger.info("END_DATE = {}", endDate);
						int numberOfMonthsBetweenDates =
								DateUtil.getNumberOfMonthsBetweenDates(firstDate, endDate);
						logger.info("numberOfMonthsBetweenDates = {}", numberOfMonthsBetweenDates);
						// if TermPeriodNum > 4 and the loan is fully paid in 4 Month or less
						if (entry.getValue().get(0).getTermPeriodNum() > 4
								&& numberOfMonthsBetweenDates <= 4) {
							return Boolean.TRUE;
						}
					}
				}
			}
		}
		catch (Exception e) {
			logger.error("Failed to calculate number Of Months Between given Dates");
		}
		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyService#checkIScore(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public ScreeningDTO checkIScore(ScreeningDTO screeningDTO)
			throws ResourcesNotFoundException, IScoreProductConfigurationException,
			IScoreExpiryDayException, IScoreExpiryDayFailedErrorException {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// Find loan by ID
		screeningDTO.setLoanDTO(loanService.find(screeningDTO.getIdLoan()));
		// Find product data by ID
		ProductDTO productDTO = parametrageClient
				.findProductById(screeningDTO.getLoanDTO().getProductId().longValue());
		// Check your I-SCORE/PRODUCT Configuration
		if (ACMValidationUtils.isNullOrEmpty(productDTO.getMaxNumDaysExpiry())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMaxScore())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMaxActiveLoans())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMaxNumDaysDue())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMinActiveLoans())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMinNumDaysDue())
				|| ACMValidationUtils.isNullOrEmpty(productDTO.getMinScore())) {
			logger.error(CommonExceptionsMessage.ISCORE_PRODUCT_CONFIGURATION);
			throw new IScoreProductConfigurationException(
					new ExceptionResponseMessage(CommonErrorCode.ISCORE_PRODUCT_CONFIGURATION,
							CommonExceptionsMessage.ISCORE_PRODUCT_CONFIGURATION),
					CommonExceptionsMessage.ISCORE_PRODUCT_CONFIGURATION);
		}
		// Find customer by ID
		screeningDTO.setCustomerDTO(customerService.find(screeningDTO.getCustomerDTO().getId()));
		// Check if CUSTOMER can run I-SCORE check : find last Accepted I-SCORE check
		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOs =
				thirdPartyHistoriqueService.find(new ThirdPartyHistoriqueDTO(
						screeningDTO.getCustomerCategory().equals(CUSTOMER)
								? screeningDTO.getCustomerDTO().getIdentity()
								: null,
						screeningDTO.getCustomerCategory().equals(GUARANTOR)
								? screeningDTO.getCustomerDTO().getIdentity()
								: null,
						ThirdPartyCategory.ISCORE.name()));
		// init list message ERROR / FAILED
		List<String> listMessage = new ArrayList<>();
		listMessage.add(ThirdPartyStatus.ERROR.name());
		listMessage.add(ThirdPartyStatus.FAILED.name());
		// Get all thirdpartyhistorique for given customer or guarantor
		List<ThirdPartyHistoriqueDTO> thirdPartyHistoriqueDTOsErrorOrFailed = new ArrayList<>();
		thirdPartyHistoriqueDTOsErrorOrFailed = thirdPartyHistoriqueDTOs.stream().filter(
				thirdPartyErrorFailed -> listMessage.contains(thirdPartyErrorFailed.getStatus()))
				.collect(Collectors.toList());

		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTOsErrorOrFailed)) {
			if (thirdPartyHistoriqueDTOsErrorOrFailed.size() >= 2) {
				// get last ThirdParty check Historique
				ThirdPartyHistoriqueDTO lastThirdPartyHistoriqueDTOErrorOrFailed =
						thirdPartyHistoriqueDTOsErrorOrFailed.get(1);
				Date queryDateErrorFailed =
						lastThirdPartyHistoriqueDTOErrorOrFailed.getDateInsertion() != null
								? lastThirdPartyHistoriqueDTOErrorOrFailed.getDateInsertion()
								: new Date();

				Long nbDayFromLastRunErrorOrFailed =
						DateUtil.calculateDaysBetweenTwoDates(queryDateErrorFailed, new Date());
				// if size was > 2 do not execute iscore running again && last running date befor
				// one day of sysdate
				if (nbDayFromLastRunErrorOrFailed == 1) {
					logger.error(CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE_ERROR_FAILED);
					throw new IScoreExpiryDayFailedErrorException(new ExceptionResponseMessage(
							CommonErrorCode.ISCORE_INVALID_EXPIRY_DATE_FAILED_ERROR,
							CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE_ERROR_FAILED),
							CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE_ERROR_FAILED);
				}
			}
		}
		// remove all thirdparty with status ERROR and FAILED
		thirdPartyHistoriqueDTOs
				.removeIf(thirdParty -> listMessage.contains(thirdParty.getStatus()));
		if (!ACMValidationUtils.isNullOrEmpty(thirdPartyHistoriqueDTOs)) {
			Boolean checkMaxCustomerDelay = Boolean.TRUE;
			// get last ThirdParty check Historique
			ThirdPartyHistoriqueDTO lastThirdPartyHistoriqueDTO = thirdPartyHistoriqueDTOs.get(0);
			// check Identity number for given customer if it was changed
			if ((screeningDTO.getCustomerCategory().equals(CUSTOMER)
					&& !ACMValidationUtils
							.isNullOrEmpty(lastThirdPartyHistoriqueDTO.getIdentityCustomer())
					&& !ACMValidationUtils
							.isNullOrEmpty(screeningDTO.getCustomerDTO().getIdentity())
					&& (!lastThirdPartyHistoriqueDTO.getIdentityCustomer()
							.equals(screeningDTO.getCustomerDTO().getIdentity())))
					|| (screeningDTO.getCustomerCategory().equals(GUARANTOR)
							&& !ACMValidationUtils.isNullOrEmpty(
									lastThirdPartyHistoriqueDTO.getIdentityCustomerGuarantor())
							&& !ACMValidationUtils
									.isNullOrEmpty(screeningDTO.getCustomerDTO().getIdentity()))
							&& !(lastThirdPartyHistoriqueDTO.getIdentityCustomerGuarantor()
									.equals(screeningDTO.getCustomerDTO().getIdentity()))) {
				checkMaxCustomerDelay = Boolean.FALSE;
			}
			// calculate Days Between Two current day && last query date
			Date queryDate = lastThirdPartyHistoriqueDTO.getDateInsertion() != null
					? lastThirdPartyHistoriqueDTO.getDateInsertion()
					: new Date();

			Long nbDayFromLastRun = DateUtil.calculateDaysBetweenTwoDates(queryDate, new Date());
			// check last RUN history and compare : MAXIMUM CUSTOMER DELAY : DEFAULT = 30 days
			if (Boolean.TRUE.equals(checkMaxCustomerDelay)
					&& nbDayFromLastRun < productDTO.getMaxNumDaysExpiry()) {
				logger.error(CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE);
				throw new IScoreExpiryDayException(
						new ExceptionResponseMessage(CommonErrorCode.ISCORE_INVALID_EXPIRY_DATE,
								CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE),
						CommonExceptionsMessage.ISCORE_INVALID_EXPIRY_DATE);
			}
		}

		try {
			// Send I-SCORE Request
			// USE SOAP API
			// ScreeningDTO responseScreeningDTO =
			// transversClient.requestSOAPIScoreAndGenerateIScoreReport(screeningDTO);
			// USE REST API
			ScreeningDTO responseScreeningDTO =
					thirdPartyTransversClient.sendIScoreRequest(screeningDTO);

			logger.info("RESPONSE SCREENING : {}", responseScreeningDTO);

			// Mapping status ISCORE
			String statusISCORE = ThirdPartyStatus.ERROR.name();
			if (!ACMValidationUtils.isNullOrEmpty(responseScreeningDTO.getXmlResponse())
					&& !(responseScreeningDTO.getXmlResponse()
							.contains(ThirdPartyStatus.ERROR.name()))) {
				statusISCORE = ThirdPartyStatus.ACCEPTED.name();
				// INIT ThirdPartyHistorique object
				ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO = saveThirdPartyHistorique(
						responseScreeningDTO, ThirdPartyCategory.ISCORE.name(),
						responseScreeningDTO.getXmlRequest(), responseScreeningDTO.getXmlResponse(),
						statusISCORE, productDTO, null);
				responseScreeningDTO.setThirdPartyHistoriqueDTO(thirdPartyHistoriqueDTO);
				// Processing Loan Status based on I-SCORE result => ACCEPTED / REJECTED / RISK
				statusISCORE = processingIScoreResponse(productDTO, thirdPartyHistoriqueDTO,
						screeningDTO.getLoanDTO(), screeningDTO.getCustomerCategory());

				// Setting third Party history new Status if is not ACCEPTED
				if (!(statusISCORE.equals(ThirdPartyStatus.ACCEPTED.name()))) {
					thirdPartyHistoriqueDTO.setStatus(statusISCORE);
					thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO.getId(),
							thirdPartyHistoriqueDTO);
				}
				// *** SAVE GENERATED REPORT IN GED ***
				logger.info("PDFStream : {}", responseScreeningDTO.getIscoreReport());
				if (!ACMValidationUtils.isNullOrEmpty(responseScreeningDTO.getIscoreReport())) {
					sendIscoreByteToGED(thirdPartyHistoriqueDTO, responseScreeningDTO);
				}
			}
			else if (!ACMValidationUtils.isNullOrEmpty(responseScreeningDTO.getXmlResponse())
					&& (responseScreeningDTO.getXmlResponse()
							.contains(ThirdPartyStatus.ERROR.name()))) {
				statusISCORE = ThirdPartyStatus.FAILED.name();
				// init ThirdPartyHistorique object
				responseScreeningDTO.setThirdPartyHistoriqueDTO(saveThirdPartyHistorique(
						responseScreeningDTO, ThirdPartyCategory.ISCORE.name(),
						responseScreeningDTO.getXmlRequest(), responseScreeningDTO.getXmlResponse(),
						statusISCORE, productDTO, null));
			}
			else {
				// returning with FAILED status
				responseScreeningDTO.setThirdPartyHistoriqueDTO(saveThirdPartyHistorique(
						responseScreeningDTO, ThirdPartyCategory.ISCORE.name(),
						ACMValidationUtils.isNullOrEmpty(responseScreeningDTO.getXmlRequest())
								? "No Request"
								: responseScreeningDTO.getXmlRequest(),
						ACMValidationUtils.isNullOrEmpty(responseScreeningDTO.getXmlResponse())
								? ThirdPartyStatus.ERROR.name()
								: responseScreeningDTO.getXmlResponse(),
						statusISCORE, productDTO, null));
			}
			// Setting status
			responseScreeningDTO.setDecision(statusISCORE);
			// Returning data
			return responseScreeningDTO;
		}
		catch (Exception e) {
			logger.error("Error will calling ISCORE API WS");
			logger.error(" {} ", e.getMessage());
			screeningDTO.setThirdPartyHistoriqueDTO(saveThirdPartyHistorique(screeningDTO,
					ThirdPartyCategory.ISCORE.name(), "No Request", ThirdPartyStatus.ERROR.name(),
					ThirdPartyStatus.ERROR.name(), null, 0));
		}

		// Setting status
		screeningDTO.setDecision(ThirdPartyStatus.FAILED.name());
		return screeningDTO;
	}

	/**
	 * Save third party historique.
	 *
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @param category the category
	 * @param request the request
	 * @param response the response
	 * @param status the status
	 * @param productDTO the product DTO
	 * @param amlPourcentage the aml pourcentage
	 * @return the third party historique DTO
	 */
	private ThirdPartyHistoriqueDTO saveThirdPartyHistorique(ScreeningDTO screeningDTO,
			String category, String request, String response, String status, ProductDTO productDTO,
			Integer amlPourcentage) {

		// init ThirdPartyHistorique object
		ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO =
				new ThirdPartyHistoriqueDTO(screeningDTO.getIdLoan(),
						screeningDTO.getCustomerCategory().equals(CUSTOMER)
								? screeningDTO.getCustomerDTO().getId()
								: null,
						screeningDTO.getCustomerCategory().equals(GUARANTOR)
								? screeningDTO.getCustomerDTO().getId()
								: null,
						category, status, request, response,
						screeningDTO.getCustomerCategory().equals(CUSTOMER)
								? screeningDTO.getCustomerDTO().getIdentity()
								: null,
						screeningDTO.getCustomerCategory().equals(GUARANTOR)
								? screeningDTO.getCustomerDTO().getIdentity()
								: null);
		// Store additional DATA if we run an AML check
		if (category.equals(ThirdPartyCategory.AML.name())) {
			thirdPartyHistoriqueDTO.setAmlPourcentage(amlPourcentage);
		}
		// Store additional DATA if we run an I-SCORE check
		else if (category.equals(ThirdPartyCategory.ISCORE.name())
				&& !ACMValidationUtils.isNullOrEmpty(productDTO)) {
			// Setting التقييم الرقمي
			String score = screeningDTO.getScore();
			thirdPartyHistoriqueDTO.setScore(
					!ACMValidationUtils.isNullOrEmpty(score) ? Integer.parseInt(score) : 0);
			// Setting عدد التسهيلات السارية
			String activeLoan = screeningDTO.getActiveLoan();
			thirdPartyHistoriqueDTO.setActiveLoans(
					!ACMValidationUtils.isNullOrEmpty(activeLoan) ? Integer.parseInt(activeLoan)
							: 0);
			// Setting اقصى مدة تأخير للعميل
			String maxNumDaysDue = screeningDTO.getMaxNumDaysDue();
			thirdPartyHistoriqueDTO
					.setMaxNumDaysDue(!ACMValidationUtils.isNullOrEmpty(maxNumDaysDue)
							? Integer.parseInt(maxNumDaysDue)
							: 0);
			// Setting تاريخ الاستعلام
			thirdPartyHistoriqueDTO.setQueryDate(new Date());
			// Setting TotalApprovalAmt
			thirdPartyHistoriqueDTO.setTotalApprovalAmt(
					!ACMValidationUtils.isNullOrEmpty(screeningDTO.getTotalApprovalAmt())
							? screeningDTO.getTotalApprovalAmt()
							: "0");
			// Setting TotalBalanceAmount
			thirdPartyHistoriqueDTO.setTotalBalanceAmount(
					!ACMValidationUtils.isNullOrEmpty(screeningDTO.getTotalBalanceAmount())
							? screeningDTO.getTotalApprovalAmt()
							: "0");
			// Setting TotalMonthlyInstallmentAmt
			thirdPartyHistoriqueDTO.setTotalMonthlyInstallmentAmt(
					!ACMValidationUtils.isNullOrEmpty(screeningDTO.getTotalMonthlyInstallmentAmt())
							? screeningDTO.getTotalApprovalAmt()
							: "0");
		}

		// save third Party Historique
		return thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO);
	}

	/**
	 * Gets the I-SCORE status and process Loan.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @param loanDTO the loan DTO
	 * @param categoryCustomer the category customer
	 * @return the string
	 * @throws ApiAbacusException the api abacus exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private String processingIScoreResponse(ProductDTO productDTO,
			ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO, LoanDTO loanDTO,
			String categoryCustomer)
			throws ApiAbacusException, ResourcesNotFoundException, IOException {

		String status = ThirdPartyStatus.REJECTED.name();
		// If API Call ACCEPTED and return DATA
		if (thirdPartyHistoriqueDTO.getStatus().equals(ThirdPartyStatus.ACCEPTED.name())) {
			// Setting status using PRODUCT config
			Boolean statusScore = Boolean.FALSE;
			Boolean statusActiveLoan = Boolean.FALSE;
			Boolean statusMaxNumDaysDue = Boolean.FALSE;

			// #### Check SCORE ####
			logger.info("Score =  {}", thirdPartyHistoriqueDTO.getScore());
			logger.info("Product MAX_SCORE  =  {}", productDTO.getMaxScore());
			logger.info("Product MIN_SCORE  =  {}", productDTO.getMinScore());

			if (thirdPartyHistoriqueDTO.getScore() == 0) {
				// score == 0 nothing to do and the loan will be in the flow
				return ThirdPartyStatus.ACCEPTED.name();
			}

			if (thirdPartyHistoriqueDTO.getScore() < productDTO.getMinScore()) {
				if (categoryCustomer.equals(CUSTOMER)) {
					// If the score is less Than MIN_SCORE for the CUSTOMER : Loan will be rejected
					loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
					// 4 :ملائمة غير للعميل الائتمانية الجدارة
					loanDTO.setCodeExternMotifRejet(4);
					loanDTO.setNote("I-Score : Check SCORE failed");
					loanService.rejected(loanDTO);
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					// If the score is less Than MIN_SCORE for the GUARANTOR : Link will be disable
					status = ThirdPartyStatus.REJECTE_GUARANTOR.name();
				}
				logger.info("After Check SCORE : STATUS =  {}", status);
				return status;
			}
			else if (thirdPartyHistoriqueDTO.getScore() >= productDTO.getMinScore()
					&& thirdPartyHistoriqueDTO.getScore() <= productDTO.getMaxScore()) {
				// If the score is above MAX_SCORE : LOAN will be assigned to Group Risk.
				// init messsage notif
				String actionDescription = "-- SCREENING -- ISCORE checked at"
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ loanDTO.getOwnerName();
				// assign loan to RISK MANAGER GROUP
				setLoanToGroupOfUsers(CommonConstants.RISK_MANAGER, loanDTO);
				// Send notification to RISK MANAGER GROUP
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}

				if (categoryCustomer.equals(CUSTOMER)) {
					status = ThirdPartyStatus.REVIEW.name();
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					status = ThirdPartyStatus.REVIEW_GUARANTOR.name();
				}
				logger.info("After Check SCORE : STATUS =  {}", status);
				return status;
			}
			else if (thirdPartyHistoriqueDTO.getScore() > productDTO.getMaxScore()) {
				// If the score is between MIN_SCORE and MAX_SCORE it will follow the normal process
				statusScore = Boolean.TRUE;
			}

			// #### Check ACTIVE_LOANS ####
			logger.info("ActiveLoans =  {}", thirdPartyHistoriqueDTO.getActiveLoans());
			logger.info("Product MAX_ACTIVE_LOANS  =  {}", productDTO.getMaxActiveLoans());
			logger.info("Product MIN_ACTIVE_LOANS  =  {}", productDTO.getMinActiveLoans());
			if (Boolean.TRUE.equals(statusScore)
					&& thirdPartyHistoriqueDTO.getActiveLoans() <= productDTO.getMinActiveLoans()) {
				// if MIN_ACTIVE_LOANS >= active_loans : follow the normal process
				statusActiveLoan = Boolean.TRUE;
			}
			else if (Boolean.TRUE.equals(statusScore)
					&& thirdPartyHistoriqueDTO.getActiveLoans() > productDTO.getMinActiveLoans()
					&& thirdPartyHistoriqueDTO.getActiveLoans() <= productDTO.getMaxActiveLoans()) {
				// If MIN_ACTIVE_LOANS <= active_loans <= MAX_ACTIVE_LOANS : assigned to Group Risk.
				// init messsage notif
				String actionDescription = "-- SCREENING -- ISCORE checked at"
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ loanDTO.getOwnerName();
				// assign loan to RISK MANAGER GROUP
				setLoanToGroupOfUsers(CommonConstants.RISK_MANAGER, loanDTO);
				// Send notification to RISK MANAGER GROUP
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}

				if (categoryCustomer.equals(CUSTOMER)) {
					status = ThirdPartyStatus.REVIEW.name();
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					status = ThirdPartyStatus.REVIEW_GUARANTOR.name();
				}
				logger.info("After Check ACTIVE_LOANS : STATUS =  {}", status);
				return status;
			}
			else if (Boolean.TRUE.equals(statusScore)
					&& thirdPartyHistoriqueDTO.getActiveLoans() > productDTO.getMaxActiveLoans()) {
				if (categoryCustomer.equals(CUSTOMER)) {
					// If active_loans > MAX_ACTIVE_LOANS for the CUSTOMER : Loan will be rejected
					loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
					// 4 :ملائمة غير للعميل الائتمانية الجدارة
					loanDTO.setCodeExternMotifRejet(4);
					loanDTO.setNote("I-Score : Check ACTIVE_LOANS failed");
					loanService.rejected(loanDTO);
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					// If active_loans > MAX_ACTIVE_LOANSE for the GUARANTOR : Link will be disable
					status = ThirdPartyStatus.REJECTE_GUARANTOR.name();
				}
				logger.info("After Check ACTIVE_LOANS : STATUS =  {}", status);
				return status;
			}

			// #### Check MAX_NUM_DAYS_DUE ####
			logger.info("MaxNumDaysDue =  {}", thirdPartyHistoriqueDTO.getMaxNumDaysDue());
			logger.info("Product MAX_NUM_DAYS_DUE  =  {}", productDTO.getMaxNumDaysDue());
			logger.info("Product MIN_NUM_DAYS_DUE  =  {}", productDTO.getMinNumDaysDue());
			if (Boolean.TRUE.equals(statusScore) && Boolean.TRUE.equals(statusActiveLoan)
					&& thirdPartyHistoriqueDTO.getMaxNumDaysDue() < productDTO.getMinNumDaysDue()) {
				// Number of late days is less than MIN_NUM_DAYS_DUE days : Accepted
				statusMaxNumDaysDue = Boolean.TRUE;
			}
			else if (Boolean.TRUE.equals(statusScore) && Boolean.TRUE.equals(statusActiveLoan)
					&& thirdPartyHistoriqueDTO.getMaxNumDaysDue() >= productDTO.getMinNumDaysDue()
					&& thirdPartyHistoriqueDTO.getMaxNumDaysDue() <= productDTO
							.getMaxNumDaysDue()) {
				// NUM_DAYS is between MIN_NUM_DAYS_DUE and MAX_NUM_DAYS_DUE : assigned to Group
				// Risk
				// init messsage notif
				String actionDescription = "-- SCREENING -- ISCORE checked at"
						+ DateUtil.formatDate(new Date(), CommonConstants.PATTREN_DATE) + " By "
						+ loanDTO.getOwnerName();
				// assign loan to RISK MANAGER GROUP
				setLoanToGroupOfUsers(CommonConstants.RISK_MANAGER, loanDTO);
				// Send notification to RISK MANAGER GROUP
				if (loanDTO.getAssignedToOneUser()) {
					notifyOneUserPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}
				else {
					notifyUsersPerGroup(loanDTO, CommonConstants.RISK_MANAGER, actionDescription);
				}

				if (categoryCustomer.equals(CUSTOMER)) {
					status = ThirdPartyStatus.REVIEW.name();
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					status = ThirdPartyStatus.REVIEW_GUARANTOR.name();
				}
				logger.info("After Check MAX_NUM_DAYS_DUE : STATUS =  {}", status);
				return status;
			}
			else if (Boolean.TRUE.equals(statusScore) && Boolean.TRUE.equals(statusActiveLoan)
					&& thirdPartyHistoriqueDTO.getMaxNumDaysDue() > productDTO.getMaxNumDaysDue()) {
				if (categoryCustomer.equals(CUSTOMER)) {
					// If the Num of days exceed MAX_NUM_DAYS_DUE for the CUSTOMER : Loan will be
					// rejected
					loanDTO.setIsNotFromWorkflow(Boolean.TRUE);
					// 4 :ملائمة غير للعميل الائتمانية الجدارة
					loanDTO.setCodeExternMotifRejet(4);
					loanDTO.setNote("I-Score : Check MAX_NUM_DAYS_DUE failed");
					loanService.rejected(loanDTO);
				}
				else if (categoryCustomer.equals(GUARANTOR)) {
					// If the Num of days exceed MAX_NUM_DAYS_DUE for the GUARANTOR : Link will be
					// disable
					status = ThirdPartyStatus.REJECTE_GUARANTOR.name();
				}
				logger.info("After Check MAX_NUM_DAYS_DUE : STATUS =  {}", status);
				return status;
			}

			// Check if all CONFIG are respected
			if (Boolean.TRUE.equals(statusScore) && Boolean.TRUE.equals(statusActiveLoan)
					&& Boolean.TRUE.equals(statusMaxNumDaysDue)) {
				status = ThirdPartyStatus.ACCEPTED.name();
			}
		}
		logger.info("I-SCORE STATUS =  {}", status);
		return status;
	}

	/**
	 * Gets the default owner risk. Sets the loan to group of users.
	 *
	 * @author idridi
	 * @param groupeCode the groupe code
	 * @param loanDTO the loan DTO
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private LoanDTO setLoanToGroupOfUsers(String groupeCode, LoanDTO loanDTO)
			throws ResourcesNotFoundException {

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
			// set group owner and group owner name
			loanDTO.setGroupOwner(groupeDTO.getCode());
			loanDTO.setGroupOwnerName(groupeDTO.getLibelle());
			loanDTO.setAssignedToOneUser(Boolean.FALSE);
		}

		loanService.save(loanDTO.getLoanId(), loanDTO);
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
			NotificationsDTO notificationsDTO = new NotificationsDTO();
			// list of users
			for (UserDTO userDTO : userDTOs) {
				if (groupeCode.equals(CommonConstants.RISK_MANAGER)) {
					notificationsDTO =
							notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE, CommonConstants.ACM_NOTIFICATION_ACTION_RISK,
									actionDescription, loanDTO, null));
					logger.info("New Risk Notification [{}] has been inserted.", notificationsDTO);
				}
				else if (groupeCode.equals(CommonConstants.COMPLIANCE_GROUP)) {
					notificationsDTO =
							notificationsServices.save(new NotificationsDTO(userDTO.getLogin(),
									NotificationCategory.LOAN.name(), NotificationType.INFO.name(),
									Boolean.TRUE,
									CommonConstants.ACM_NOTIFICATION_ACTION_COMPLIANCE,
									actionDescription, loanDTO, null));
					logger.info("New Compliance Notification [{}] has been inserted.",
							notificationsDTO);
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
		if (groupeCode.equals(CommonConstants.RISK_MANAGER)) {
			notificationsDTO = notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
					NotificationCategory.LOAN.name(), NotificationType.INFO.name(), Boolean.TRUE,
					CommonConstants.ACM_NOTIFICATION_ACTION_RISK, actionDescription, loanDTO,
					null));
			logger.info("New Risk Notification [{}] has been inserted.", notificationsDTO);
		}
		else if (groupeCode.equals(CommonConstants.COMPLIANCE_GROUP)) {
			notificationsDTO = notificationsServices.save(new NotificationsDTO(loanDTO.getOwner(),
					NotificationCategory.LOAN.name(), NotificationType.INFO.name(), Boolean.TRUE,
					CommonConstants.ACM_NOTIFICATION_ACTION_COMPLIANCE, actionDescription, loanDTO,
					null));
			logger.info("New Compliance Notification [{}] has been inserted.", notificationsDTO);
		}
	}

	/**
	 * Send iscore byte to GED.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @param screeningDTO the screening DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	private void sendIscoreByteToGED(ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO,
			ScreeningDTO screeningDTO) throws ResourcesNotFoundException {

		// ** SAVE GENERATED REPORT IN GED **
		// init data to be send to GED
		GedParameterDTO gedParameterDTO = new GedParameterDTO();
		gedParameterDTO.setPath(CommonConstants.APP_CLIENT);
		gedParameterDTO.setSite(CommonConstantGED.ACM_SITE);
		// setting tags
		List<String> tags = new ArrayList<>();
		Long idCustomer = thirdPartyHistoriqueDTO.getIdCustomer() != null
				? thirdPartyHistoriqueDTO.getIdCustomer()
				: thirdPartyHistoriqueDTO.getIdCustomerGuarantor();
		// find customer data
		CustomerDTO customerDTO = customerService.findCustomer(idCustomer);
		// INIT tag = ISCORE - CUSTOMER_NUMBER - ID_THIRD_PARTY_HISTORIQUE
		tags.add(ThirdPartyCategory.ISCORE.name() + "-" + customerDTO.getCustomerNumber() + "-"
				+ thirdPartyHistoriqueDTO.getId());
		// set customer number
		gedParameterDTO.setCustomerNumber(customerDTO.getCustomerNumber());
		gedParameterDTO.setTags(tags);
		// convert byte to File
		List<File> filesToSend = new ArrayList<>();
		filesToSend.add(CommonFunctions.fileConverter(screeningDTO.getIscoreReport(),
				environment.getProperty("spring.servlet.multipart.location"),
				ThirdPartyCategory.ISCORE.name()));
		gedParameterDTO.setFilesToUpload(filesToSend);
		try {
			// send to GED
			List<String> ids = gedClient.uploadToGed(gedParameterDTO);
			String idDocumentGED =
					ACMValidationUtils.isNullOrEmpty(ids) ? null : ids.get(0).split(";")[0];
			logger.info("INSERTED ID RREPORT IN GED : {} ", idDocumentGED);
			// update THIRD PARTY HISTORIQUE
			thirdPartyHistoriqueDTO.setReportTag(idDocumentGED);
			thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO.getId(),
					thirdPartyHistoriqueDTO);
		}
		catch (Exception e) {
			logger.error("FAILED TO SAVE IN GED");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyService#sendKycGetPersonDetails(com.acm.soap.kyc.model.KycDTO)
	 */
	@Override
	public KycDTO sendKycGetPersonDetails(KycDTO kycDTO) {

		logger.info("START methode sendKycGetPersonDetails");
		Preconditions.checkNotNull(kycDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		KycDTO soapKycDTO = new KycDTO();
		try {
			soapKycDTO = transversClient.sendRequestSOAPKycGetPersonDetails(kycDTO);
			logger.info("GetPersonDetails Response : {}", soapKycDTO.toString());

			// save ThirdParty Historique
			saveThirdPartyHistorique(new ScreeningDTO(), ThirdPartyCategory.KYC.name(),
					soapKycDTO.getXmlRequest(), soapKycDTO.getXmlResponse().toString(),
					soapKycDTO.getXmlResponse().getErrorDescription(), null, null);
		}
		catch (Exception e) {
			logger.error("FAILED TO SAVE IN GED");
			logger.error(CommonLoggerMessage.ERROR_WHILE_CONNECTING_TO_REMOTE_SERVICE,
					e.getMessage());
		}
		return soapKycDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ThirdPartyService#getThirdPartyMapping(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public ThirdPartyMappingData getThirdPartyMapping(String nationality, String category) {

		return thirdPartyMappingDataRepository.findByOriginalDataAndCategory(nationality, category);
	}
}
