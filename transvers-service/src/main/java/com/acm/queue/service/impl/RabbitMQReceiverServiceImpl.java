/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.queue.service.impl;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.acm.api_abacus.service.LoanCreateUpdateApiService;
import com.acm.api_abacus.service.LoanScheduleAPIService;
import com.acm.client.CreditClient;
import com.acm.client.ParametrageClient;
import com.acm.client.TransversClient;
import com.acm.constants.common.ACMConstantWorkflowStatuts;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.queue.service.RabbitMQReceiverService;
import com.acm.utils.dtos.ClaimNoteDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanScheduleDTO;
import com.acm.utils.dtos.LoanSchedulesApiDTO;
import com.acm.utils.dtos.PaymentApiAbacusDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.ScheduleApiDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * The RabbitMQReceiverServiceImpl class is an implementation of the RabbitMQReceiverService
 * interface. It provides methods for receiving and processing messages from RabbitMQ queues. This
 * class uses Spring AMQP's RabbitListener annotation for message consumption. It logs the received
 * messages using SLF4J logging framework.
 *
 * @author nrmila
 * @see RabbitMQReceiverService
 * @since 1.0.8
 */
@Service
public class RabbitMQReceiverServiceImpl implements RabbitMQReceiverService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(RabbitMQReceiverServiceImpl.class);

	/** The credit client. */
	@Autowired
	private CreditClient creditClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The loan schedule api service. */
	@Autowired
	private LoanScheduleAPIService loanScheduleApiService;

	/** The loan schedule api service. */
	@Autowired
	private TransversClient transversClient;

	/** The loan queue topic. */
	@Value("${ib.rabbitmq.loan}")
	private String loanQueueTopic;

	/** The customer queue topic. */
	@Value("${ib.rabbitmq.customer}")
	private String customerQueueTopic;

	/** The token. */
	private static String token = "";

	/** The token date. */
	private static DateTime tokenDate = DateTime.now();

	/** The url server authentication. */
	@Value("${url.serveur.authentification}")
	private String urlServeurAuthentification;

	/** The loan create update api service. */
	@Autowired
	private LoanCreateUpdateApiService loanCreateUpdateApiService;

	/**
	 * Received customer.
	 *
	 * @param idIbCustomer the id ib customer
	 */
	@Override
	public void receivedCustomer(String idIbCustomer) {

		logger.info("Received customer Message From RabbitMQ idIbCustomer: {} ", idIbCustomer);
		CustomerDTO newCustomer = null;
		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}

			// find customer in ACM by idIbCustomer
			List<CustomerDTO> acmCustomerDTOs =
					creditClient.findCustomerByIbCustomerId(Long.parseLong(idIbCustomer), token);
			// find customer from IB
			CustomerDTO customerParam = new CustomerDTO();
			customerParam.setIbCustomerId(Long.parseLong(idIbCustomer));
			List<CustomerDTO> ibCustomerDTOs =
					creditClient.findAllCustomerInformationInIb(customerParam, token);

			ibCustomerDTOs.get(0).setCheckCustomer(Boolean.FALSE);
			if (!ACMValidationUtils.isNullOrEmpty(acmCustomerDTOs)) {
				if (ACMValidationUtils
						.isNullOrEmpty(ibCustomerDTOs.get(0).getCustomerLinksRelationshipDTOs())) {
					ibCustomerDTOs.get(0).setCustomerLinksRelationshipDTOs(new ArrayList<>());
				}
				if (ACMValidationUtils
						.isNullOrEmpty(ibCustomerDTOs.get(0).getUserDefinedFieldsLinksDTOs())) {
					ibCustomerDTOs.get(0).setUserDefinedFieldsLinksDTOs(new ArrayList<>());
				}
				// update the customer
				creditClient.customerUpdateForApplication(ibCustomerDTOs.get(0), token);

			}
			else {
				// save new customer
				if (ACMValidationUtils
						.isNullOrEmpty(ibCustomerDTOs.get(0).getCustomerLinksRelationshipDTOs())) {
					ibCustomerDTOs.get(0).setCustomerLinksRelationshipDTOs(new ArrayList<>());
				}
				if (ACMValidationUtils
						.isNullOrEmpty(ibCustomerDTOs.get(0).getUserDefinedFieldsLinksDTOs())) {
					ibCustomerDTOs.get(0).setUserDefinedFieldsLinksDTOs(new ArrayList<>());
				}

				newCustomer = creditClient.customerSaveForApplication(ibCustomerDTOs.get(0), token);
			}
			if (!ACMValidationUtils.isNullOrEmpty(newCustomer)) {
				// find loan in IB of this customer (if loan exist) ,then save it in acm
				LoanDTO dto = new LoanDTO();
				dto.setCustomerDTO(customerParam);
				List<LoanDTO> ibLoanDTOs = creditClient.findLoanInIb(dto, token);
				// TO DO : check if the loan exist in acm
				if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTOs)) {
					// find loan in acm
					List<LoanDTO> acmLoanDTOs =
							creditClient.findByIdIbLoan(ibLoanDTOs.get(0).getIdIbLoan(), token);
					if (ACMValidationUtils.isNullOrEmpty(acmLoanDTOs)) {
						// save loan in ACM
						if (!ACMValidationUtils.isNullOrEmpty(ibLoanDTOs.get(0).getProductId())) {
							ProductDTO productDto;
							productDto = parametrageClient.findProductById(
									ibLoanDTOs.get(0).getProductId().longValue(), token);
							ibLoanDTOs.get(0).setProductDTO(productDto);
						}
						// save loan in ACM
						ibLoanDTOs.get(0).setCustomerDTO(newCustomer);
						if (!ACMValidationUtils
								.isNullOrEmpty(ibLoanDTOs.get(0).getUserDefinedFieldsLinksDTOs())) {
							ibLoanDTOs.get(0).setUserDefinedFieldsLinksDTOs(new ArrayList<>());
						}
						LoanDTO loanDTO =
								creditClient.loanSaveForApplication(ibLoanDTOs.get(0), token);
						// update loan in IB
						// loanDTO.setStepPath(ibLoanDTOs.get(0).getStepPath());
						creditClient.updateAcmLoanAndCustomerInIB(loanDTO, token);
					}

				}

			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}

		logger.info("treated customer From RabbitMQ acm.queue: {}", idIbCustomer);
	}

	/**
	 * Received loan.
	 *
	 * @param message the message
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.queue.service.RabbitMQReceiverService#receivedLoan(java.lang.String)
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.loan}")
	public void receivedLoan(String message) {

		// message = "31313;APPROVE_SIGN";
		String[] keyValueMap = message.split(";");
		String idIbLoan = keyValueMap[0];
		String action = keyValueMap[1];

		logger.info("Received loan Message From RabbitMQ idIbLoan : {} ", idIbLoan);
		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}
			// find loan in ib
			LoanDTO loanDTO = new LoanDTO();
			loanDTO.setIdIbLoan(Long.parseLong(idIbLoan));
			List<LoanDTO> ibLoanDTOs = creditClient.findLoanInIb(loanDTO, token);
			// find customer in ACM by idIbCustomer
			List<CustomerDTO> acmCustomerDTOs = creditClient.findCustomerByIbCustomerId(
					(ibLoanDTOs.get(0).getCustomerDTO().getIbCustomerId()), token);
			CustomerDTO customerDTO = new CustomerDTO();

			// find loan in acm
			List<LoanDTO> acmLoanDTOs =
					creditClient.findByIdIbLoan(Long.parseLong(idIbLoan), token);

			if (!ACMValidationUtils.isNullOrEmpty(acmLoanDTOs)) {
				// update loan in ACM
				ibLoanDTOs.get(0).setCustomerDTO(acmLoanDTOs.get(0).getCustomerDTO());
				if (ACMValidationUtils
						.isNullOrEmpty(ibLoanDTOs.get(0).getUserDefinedFieldsLinksDTOs())) {
					ibLoanDTOs.get(0).setUserDefinedFieldsLinksDTOs(new ArrayList<>());
				}
				ibLoanDTOs.get(0).setSendToIb(Boolean.FALSE);

				LoanDTO updatedLoan = creditClient.updateLoan(ibLoanDTOs.get(0), token);

				if (action.equals("APPROVE")) {
					updatedLoan.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
					creditClient.validateLoan(updatedLoan, token);
				}
				else if (action.equals("CUSTOMERDECISION")) {
					updatedLoan.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_CUSTOMER_DECISION);
					creditClient.validateLoan(updatedLoan, token);
				}
				else if (action.equals("CANCEL")) {
					updatedLoan.setCodeExternMotifRejet(1);
					creditClient.cancelLoan(updatedLoan, token);
				}
				else if (action.equals("APPROVE_SIGN")) {
					logger.info("INIT APPROVE_SIGN for emdha api");

					updatedLoan.setWorkflowNextAction(
							ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);

					boolean resulatEmdhaApi = transversClient.emdhaApi(updatedLoan, token);
					logger.info("resulatEmdhaApi: {}", resulatEmdhaApi);

					// if ok
					if (!ACMValidationUtils.isNullOrEmpty(resulatEmdhaApi) && resulatEmdhaApi) {
						logger.info("validateLoan for emdha api");
						creditClient.validateLoan(updatedLoan, token);

					}
				}

			}
			else {
				// if customerDTOs is empty then create customer in acm
				if (ACMValidationUtils.isNullOrEmpty(acmCustomerDTOs)) {
					// find customer from IB
					CustomerDTO customerParam = new CustomerDTO();
					customerParam
							.setIbCustomerId(ibLoanDTOs.get(0).getCustomerDTO().getIbCustomerId());
					List<CustomerDTO> ibCustomerDTOs =
							creditClient.findAllCustomerInformationInIb(customerParam, token);

					// save new customer
					if (ACMValidationUtils.isNullOrEmpty(
							ibCustomerDTOs.get(0).getCustomerLinksRelationshipDTOs())) {
						ibCustomerDTOs.get(0).setCustomerLinksRelationshipDTOs(new ArrayList<>());
					}
					if (ACMValidationUtils
							.isNullOrEmpty(ibCustomerDTOs.get(0).getUserDefinedFieldsLinksDTOs())) {
						ibCustomerDTOs.get(0).setUserDefinedFieldsLinksDTOs(new ArrayList<>());
					}

					customerDTO =
							creditClient.customerSaveForApplication(ibCustomerDTOs.get(0), token);
				}
				else {
					customerDTO = acmCustomerDTOs.get(0);
				}
				ibLoanDTOs.get(0).setCustomerDTO(customerDTO);
				creditClient.loanSaveForApplication(ibLoanDTOs.get(0), token);
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}

		logger.info("treated loan From2 RabbitMQ acm.queue: {}", idIbLoan);
	}

	/**
	 * Received validate loan.
	 *
	 * @param idIbLoan the id ib loan
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.queue.service.RabbitMQReceiverService#receivedValidateLoan(java.lang.String)
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.validate.loan}")
	public void receivedValidateLoan(String idIbLoan) {

		logger.info("Received validate loan Message From RabbitMQ idIbLoan: {} ", idIbLoan);
		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}

			// find loan in acm
			List<LoanDTO> acmLoanDTOs =
					creditClient.findByIdIbLoan(Long.parseLong(idIbLoan), token);
			if (!ACMValidationUtils.isNullOrEmpty(acmLoanDTOs)) {
				acmLoanDTOs.get(0).setWorkflowNextAction(
						ACMConstantWorkflowStatuts.WORKFLOW_REQUEST_ACTION_NEXT);
				creditClient.validateLoan(acmLoanDTOs.get(0), token);
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}

		logger.info("treated validate loan From RabbitMQ acm.queue: {}", idIbLoan);

	}

	/**
	 * get loanSchedules for IB.
	 *
	 * @param idIbCustomer the id ib loan
	 * @return the loan schedules for IB
	 * @throws ApiAbacusException the api abacus exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.queue.service.RabbitMQReceiverService#getLoanSchedulesForIB(java.lang.String)
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.loan.schedules}")
	public void getLoanSchedulesForIB(String idIbCustomer) throws ApiAbacusException {

		logger.info("Fetch loan schedules by customer From RabbitMQ acm.queue: {} ", idIbCustomer);

		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}
			List<LoanSchedulesApiDTO> loanSchedulesApiDTOs = new ArrayList<>();
			List<CustomerDTO> customers =
					creditClient.findCustomerByIbId(Long.parseLong(idIbCustomer), token);

			if (!ACMValidationUtils.isNullOrEmpty(customers)) {
				String idExternCustomer = customers.get(0).getCustomerIdExtern().toString();
				loanSchedulesApiDTOs = loanScheduleApiService.getAllSchedules(idExternCustomer);
			}

			/** Fetch Schedules from abacus and save in IB */
			if (!ACMValidationUtils.isNullOrEmpty(loanSchedulesApiDTOs)) {
				logger.info(" Loan Schedules Fetched Succesfully for customer with id: ",
						idIbCustomer);

				for (LoanSchedulesApiDTO loanScheduleData : loanSchedulesApiDTOs) {

					Long accountId = loanScheduleData.getCuAccountID();
					LoanDTO loanDTO = creditClient.findLoanByIdAccountExtern(accountId, token);

					List<ScheduleDTO> scheduleDTOs = new ArrayList<>();
					List<ScheduleApiDTO> dtos = loanScheduleData.getLoanSchedule();

					for (ScheduleApiDTO data : dtos) {
						ScheduleDTO scheduleDto = new ScheduleDTO();
						scheduleDto.setId(accountId);
						scheduleDto.setPeriod(data.getPeriod());
						scheduleDto.setTotalRepayment(data.getTotalRepayment());
						scheduleDto.setLoanRepayment(data.getLoanRepayment());
						scheduleDto.setInterestRepayment(data.getInterestRepayment());
						scheduleDto.setBalance(data.getBalance());
						scheduleDto.setStatusLabel(data.getStatus());
						scheduleDto.setNbArrearsDays(new BigDecimal(data.getLateDays()));
						scheduleDto.setInterestAmtPaid(data.getInterestAmtPaid());
						scheduleDto.setLoanRepaymentPaid(data.getLoanRepaymentPaid());
						scheduleDto.setIdLoanExtern(accountId);
						scheduleDto.setRepaidOn(data.getRepaidOn());
						scheduleDto.setPenalityDue(data.getPenaltyDue());
						scheduleDto.setPenalityPaid(data.getPenaltyDuePaid());
						scheduleDto.setLateDays(data.getLateDays());

						/** Date Sets */
						SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");

						if (data.getRepaidDate() != null && !data.getRepaidDate().isEmpty()) {
							Date repaidDate = dateFormat.parse(data.getRepaidDate());
							scheduleDto.setRepaidOn(repaidDate);
						}
						if (data.getRepaymentDate() != null && !data.getRepaymentDate().isEmpty()) {
							Date repDate = dateFormat.parse(data.getRepaymentDate());
							scheduleDto.setRepaymentDate(repDate);
						}
						if (data.getRepaidDate() != null && !data.getRepaidDate().isEmpty()) {
							Date repaidDate = dateFormat.parse(data.getRepaidDate());
							scheduleDto.setRepaidOn(repaidDate);
						}

						scheduleDTOs.add(scheduleDto);
					}
					LoanScheduleDTO loanSchedule = new LoanScheduleDTO(loanDTO, scheduleDTOs);
					creditClient.saveLoansSchedules(loanSchedule, token);
				}
			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
			throw new ApiAbacusException(CommonErrorCode.FETCH_LOANS_SCHEDULES,
					"ERROR WHILE FETCHING SCHEDULE");
		}
	}

	/**
	 * Received payment loan.
	 *
	 * @param paymentLoan the payment loan
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.queue.service.RabbitMQReceiverService#receivedPaymentLoan(java.lang.String)
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.paymentLoan}")
	public void receivedPaymentLoan(String paymentLoan) {

		String idLoan = null;
		String amount = null;
		String usernameAbacus = null;

		logger.info("Received idLoan and Amount Message From RabbitMQ paymentLoan: {} ",
				paymentLoan);

		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}

			String[] keyValueMap = paymentLoan.split(";");
			if (!ACMValidationUtils.isNullOrEmpty(keyValueMap) && keyValueMap.length == 3) {
				idLoan = keyValueMap[0];
				amount = keyValueMap[1];
				usernameAbacus = keyValueMap[2];
				// find loan in acm
				List<LoanDTO> acmLoanDTOs =
						creditClient.findByIdIbLoan(Long.parseLong(idLoan), token);
				if (!ACMValidationUtils.isNullOrEmpty(acmLoanDTOs)) {
					// if loan exist call api payment
					PaymentApiAbacusDTO paymentApiAbacusDTO = new PaymentApiAbacusDTO();
					paymentApiAbacusDTO.setAmount(Double.parseDouble(amount));
					paymentApiAbacusDTO.setNotes("Paid From Mobile");
					paymentApiAbacusDTO.setPayFee(true);
					paymentApiAbacusDTO.setAccountNumber(acmLoanDTOs.get(0).getAccountNumber());
					logger.info("object to send for payment api abacus : {} ", paymentApiAbacusDTO);
					loanCreateUpdateApiService.paymentLoan(paymentApiAbacusDTO, usernameAbacus,
							"SANAD");
				}

			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}

		logger.info("treated loan From2 RabbitMQ acm.queue: {}", idLoan);

	}

	/**
	 * Received cancel loan.
	 *
	 * @param idIbLoan the id ib loan
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.queue.service.RabbitMQReceiverService#receivedCancelLoan(java.lang.String)
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.cancel.loan}")
	public void receivedCancelLoan(String idIbLoan) {

		logger.info("Received cancel loan Message From RabbitMQ idIbLoan: {} ", idIbLoan);
		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);

			}
			// find loan in acm
			List<LoanDTO> acmLoanDTOs =
					creditClient.findByIdIbLoan(Long.parseLong(idIbLoan), token);
			if (!ACMValidationUtils.isNullOrEmpty(acmLoanDTOs)) {
				// cancel loan in acm
				acmLoanDTOs.get(0).setCodeExternMotifRejet(1);
				acmLoanDTOs.get(0).setNote(null);
				creditClient.cancelLoan(acmLoanDTOs.get(0), token);

			}

		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}

		logger.info("treated cancel loan From RabbitMQ acm.queue with idIbLoan: {}", idIbLoan);
	}

	/**
	 * Received note.
	 *
	 * @param claimNoteDTO the claim note DTO
	 */
	@Override
	@RabbitListener(queues = "${ib.rabbitmq.send.note}")
	public void receivedNote(ClaimNoteDTO claimNoteDTO) {

		logger.info("Received note Message From RabbitMQ acm.queue with claim id : {}",
				claimNoteDTO.getClaimId());
		try {
			logger.info("Log Existing Token : " + token);
			if (ACMValidationUtils.isNullOrEmpty(token)
					|| tokenDate.plusMinutes(30).isBeforeNow()) {
				logger.info("Generate new Token for RMQ user");
				token = "Bearer " + CommonFunctions.generateTokenForLogin(
						urlServeurAuthentification, "rabbitMq.user", "Talys@123");
				tokenDate = DateTime.now();
				logger.info("New Token : " + token);
			}
			if (!ACMValidationUtils.isNullOrEmpty(claimNoteDTO)) {
				parametrageClient.saveNoteFromIb(claimNoteDTO, token);
			}
		}
		catch (Exception e) {
			logger.error(e.getMessage());
			e.printStackTrace();
		}
		logger.info("treated send note From RabbitMQ acm.queue with claim id : {}",
				claimNoteDTO.getClaimId());
	}

}
