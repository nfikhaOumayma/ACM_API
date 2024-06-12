/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.service.impl;

import java.io.StringReader;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.acm.client.ParametrageClient;
import com.acm.constants.common.CommonConstants;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.soap.model.GetXMLResWithPDF;
import com.acm.soap.model.GetXMLResWithPDFResponse;
import com.acm.soap.model.ObjectFactory;
import com.acm.soap.model.PostRequest;
import com.acm.soap.model.PostRequestResponse;
import com.acm.soap.service.CheckActionService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.ScreeningDTO;
import com.acm.utils.enums.CustomerType;
import com.acm.utils.enums.ThirdPartyStatus;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link CheckActionServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.0
 */
@Service
public class CheckActionServiceImpl implements CheckActionService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(CheckActionServiceImpl.class);

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The soap client. */
	@Autowired
	private SoapClient soapClient;

	/** The message source. */
	@Autowired
	private MessageSource messageSource;

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.soap.service.CheckActionService#requestSOAPIScore(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public ScreeningDTO requestSOAPIScore(ScreeningDTO screeningDTO) {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Start method : requestSOAPIScore() => {}", screeningDTO);
		// Load strUserID | strPassword | SOAPPath
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_SOAP_REQUEST"));
		String strUserID = "";
		String strPassword = "";
		String pathServiceSOAP = "";
		// getting config
		for (AcmEnvironnementDTO acmEnvironnementDTO : environnementDTOs) {
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_USER"))) {
				// mf0018000100013ab
				strUserID = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PASS"))) {
				// Tamkeen1234
				strPassword = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PATH"))) {
				pathServiceSOAP = acmEnvironnementDTO.getValue();
			}
		}

		// init params
		initRequest(screeningDTO, strUserID, strPassword, pathServiceSOAP);

		// init & setting request params
		ObjectFactory objectFactory = new ObjectFactory();
		PostRequest postRequest = objectFactory.createPostRequest();
		postRequest.setStrUserID(strUserID);
		postRequest.setStrPassword(strPassword);
		postRequest.setStrRequest(screeningDTO.getXmlRequest());
		try {
			logger.info("### Send I-SCORE Request using USER = [{}] && PASS = [{}] ### ", strUserID,
					strPassword);
			logger.info("### Send I-SCORE Request TO PATH_SERVICE_SOAP = [{}] ### ",
					pathServiceSOAP);
			// call SOAP Service
			PostRequestResponse response =
					soapClient.getPostRequestResponse(postRequest, pathServiceSOAP);
			logger.debug("body Response I-SCORE = {} ",
					response != null ? response.getPostRequestResult() : "NONE");
			// setting returned DATA
			screeningDTO.setXmlResponse(response.getPostRequestResult());
		}
		catch (Exception e) {
			logger.error("{}", e.getMessage());
			e.printStackTrace();
		}
		logger.info("Execution method : requestSOAPIScore() :: DONE");
		return screeningDTO;
	}

	/**
	 * Inits the request.
	 * 
	 * @author HaythemBenizid
	 * @param screeningDTO the screening DTO
	 * @param strUserID the str user ID
	 * @param strPassword the str password
	 * @param pathServiceSOAP the path service SOAP
	 * @return the screening DTO
	 */
	private ScreeningDTO initRequest(ScreeningDTO screeningDTO, String strUserID,
			String strPassword, String pathServiceSOAP) {

		// Check if all config exist
		if (!ACMValidationUtils.isNullOrEmpty(strUserID)
				&& !ACMValidationUtils.isNullOrEmpty(strPassword)
				&& !ACMValidationUtils.isNullOrEmpty(pathServiceSOAP)) {
			// init instant datetime
			Date date = Calendar.getInstance().getTime();
			DateFormat dateFormat = new SimpleDateFormat(CommonConstants.PATTREN_DATE);
			// REQUEST_ID="{0}" - Mandatory : FIXED = 1
			String requestId = "1";

			// REPORT_ID="{1}" - Mandatory
			// Applicable Values: 3004 –INDIV / 3006- ORG
			String reportId = screeningDTO.getCustomerDTO().getCustomerType()
					.equals(CustomerType.INDIV.name()) ? "3004" : "3006";

			// SUBJECT_TYPE="{2}" - Mandatory
			// Applicable Values: 1 – Consumer (INDIV) / 0- Commercial (ORG)
			String subjectType = screeningDTO.getCustomerDTO().getCustomerType()
					.equals(CustomerType.INDIV.name()) ? "1" : "0";

			// RESPONSE_TYPE="{3}" - Mandatory : FIXED = 1
			// Applicable Value: 1 – DataPacket
			String responseType = "1";

			// LANGUAGE="{4}" - Mandatory : FIXED = 002 – Arabic
			// Applicable Values: 001 – English / 002 – Arabic
			String language = "002";

			// INQUIRY_REASON CODE="{5}" - Mandatory
			// TODO mapping
			// No. | Code | Value
			// 1. -- 1 -- Sanctioning a New Loan
			// 2. -- 8 -- Review of existing facility
			// 3. -- 3 -- Others
			// 4. -- 9 -- Review of a new facility for existing customer
			String inquiryReasonCode = "1";

			// APPLICATION PRODUCT="{6}" - Mandatory
			// TODO mapping
			String applicationProduct = "019";

			// LOAN-ACC-NO="{7}" - Conditional Mandatory
			String applicationLoanACCNo = "";

			// BANK-NAME="{8}" - Conditional Mandatory
			String applicationBankName = "";

			// BRANCH-CODE="{9}" - Conditional Mandatory
			String applicationBranchCode = "";

			// NUMBER="{10}" - Mandatory
			String applicationNumber = screeningDTO.getLoanDTO().getAccountNumber();

			// DATE="{11}" - Conditional Mandatory FIXED = Current Date
			String applicationDate = dateFormat.format(date);

			// AMOUNT="{12}" - Conditional Mandatory
			String applicationAmount = screeningDTO.getLoanDTO().getApplyAmountTotal().toString();

			// CURRENCY="{13}" - Conditional Mandatory : FIXED = EGP
			String applicationCurrency = screeningDTO.getLoanDTO().getCurrencySymbol();

			// NAME INDIVIDUAL FIRST-NAME="{14}" - Optional
			String indivFirstName = screeningDTO.getCustomerDTO().getFirstName();

			// NAME INDIVIDUAL MIDDLE-NAME="{15}" - Optional
			String indivMiddleName = screeningDTO.getCustomerDTO().getMiddleName();

			// NAME INDIVIDUAL LAST-NAME="{16}" - Mandatory
			String indivLastName = screeningDTO.getCustomerDTO().getLastName();

			// TODO IDENTIFIER IDSOURCE="{17}" - Mandatory
			String identifierIdSource = "003";

			// IDENTIFIER IDVALUE="{18}" - Mandatory
			String identifierIdValue = screeningDTO.getCustomerDTO().getIdentity();

			// SURROGATES GENDER VALUE="{19}" - Mandatory (Male:001 / Female:002)
			String genderValue = (screeningDTO.getCustomerDTO().getGender() != null
					&& screeningDTO.getCustomerDTO().getGender().equals("M")) ? "001" : "002";

			// SURROGATES DOB VALUE="{20}" - Mandatory : format="24/01/1971"
			String dateOfBirth = DateUtil.formatDate(screeningDTO.getCustomerDTO().getDateOfBirth(),
					CommonConstants.PATTREN_DATE);

			// ADDRESS TYPE="{21}" - Optional
			String addressType = "";
			// <BUILDING-NAME /> - Optional
			// <BUILDING-NUMBER /> - Optional
			// <STREET /> - Optional
			// <AREA /> - Optional
			// <CITY /> - Optional
			// <PO-BOX-NO /> - Optional
			// <GOVERNORATE /> - Optional
			// <ZIP-CODE /> - Optional
			// <E-MAIL /> - Optional
			// <PHONE /> - Optional

			// init body request params
			String[] params = {requestId, reportId, subjectType, responseType, language,
					inquiryReasonCode, applicationProduct, applicationLoanACCNo,
					applicationBankName, applicationBranchCode, applicationNumber, applicationDate,
					applicationAmount, applicationCurrency, indivFirstName, indivMiddleName,
					indivLastName, identifierIdSource, identifierIdValue, genderValue, dateOfBirth,
					addressType};

			// mapping data
			String bodyRequest = messageSource.getMessage("iscore.soap.body.request", params,
					LocaleContextHolder.getLocale());
			logger.info("bodyRequest I-SCORE = {} ", bodyRequest);
			// setting bodyRequest DATA
			screeningDTO.setXmlRequest(bodyRequest);
		}
		return screeningDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.soap.service.CheckActionService#generateIScoreReport(com.acm.utils.dtos.ScreeningDTO)
	 */
	@Override
	public byte[] generateIScoreReport(ScreeningDTO screeningDTO) {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(screeningDTO.getXmlRequest(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Start method generateIScoreReport() : XmlRequest()() => {}",
				screeningDTO.getXmlRequest());
		// Load strUserID | strPassword | SOAPPath
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_SOAP_REQUEST"));
		String strUserID = "";
		String strPassword = "";
		String pathServiceSOAP = "";
		// getting config
		for (AcmEnvironnementDTO acmEnvironnementDTO : environnementDTOs) {
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_USER"))) {
				strUserID = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PASS"))) {
				strPassword = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PATH"))) {
				pathServiceSOAP = acmEnvironnementDTO.getValue();
			}
		}
		// init & setting request params
		ObjectFactory objectFactory = new ObjectFactory();
		GetXMLResWithPDF getXMLResWithPDF = objectFactory.createGetXMLResWithPDF();
		getXMLResWithPDF.setStrUserID(strUserID);
		getXMLResWithPDF.setStrPassword(strPassword);
		getXMLResWithPDF.setStrRequest(screeningDTO.getXmlRequest());
		try {
			logger.info(
					"### Send I-SCORE generate report Request using USER = [{}] && PASS = [{}] ### ",
					strUserID, strPassword);
			logger.info("### Send I-SCORE generate report Request TO PATH_SERVICE_SOAP = [{}] ### ",
					pathServiceSOAP);
			// call SOAP Service
			GetXMLResWithPDFResponse response =
					soapClient.getGetXMLResWithPDFResponse(getXMLResWithPDF, pathServiceSOAP);
			logger.info("Execution method : generateIScoreReport() :: DONE");
			// setting returned DATA
			byte[] encoded = java.util.Base64.getEncoder()
					.encode(response.getGetXMLResWithPDFResult().getGetPDFStream());
			return java.util.Base64.getDecoder().decode(encoded);
		}
		catch (Exception e) {
			logger.error("error while generating iscore report{}", e.getMessage());
			e.printStackTrace();
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.soap.service.CheckActionService#requestSOAPIScoreAndGenerateIScoreReport(com.acm.
	 * utils.dtos.ScreeningDTO)
	 */
	@Override
	public ScreeningDTO requestSOAPIScoreAndGenerateIScoreReport(ScreeningDTO screeningDTO) {

		Preconditions.checkNotNull(screeningDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.debug("Start method : requestSOAPIScoreAndGenerateIScoreReport() => {}",
				screeningDTO);
		// Load strUserID | strPassword | SOAPPath
		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_SOAP_REQUEST"));
		String strUserID = "";
		String strPassword = "";
		String pathServiceSOAP = "";
		// getting config
		for (AcmEnvironnementDTO acmEnvironnementDTO : environnementDTOs) {
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_USER"))) {
				// mf0018000100013ab
				strUserID = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PASS"))) {
				// Tamkeen1234
				strPassword = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE
					.equals(acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_REQUEST_PATH"))) {
				pathServiceSOAP = acmEnvironnementDTO.getValue();
			}
		}

		// init params
		initRequest(screeningDTO, strUserID, strPassword, pathServiceSOAP);

		// init & setting request params
		ObjectFactory objectFactory = new ObjectFactory();
		GetXMLResWithPDF getXMLResWithPDF = objectFactory.createGetXMLResWithPDF();
		getXMLResWithPDF.setStrUserID(strUserID);
		getXMLResWithPDF.setStrPassword(strPassword);
		getXMLResWithPDF.setStrRequest(screeningDTO.getXmlRequest());
		try {
			logger.info("### Send I-SCORE Request using USER = [{}] && PASS = [{}] ### ", strUserID,
					strPassword);
			logger.info("### Send I-SCORE Request TO PATH_SERVICE_SOAP = [{}] ### ",
					pathServiceSOAP);
			// call SOAP Service
			GetXMLResWithPDFResponse response =
					soapClient.getGetXMLResWithPDFResponse(getXMLResWithPDF, pathServiceSOAP);
			logger.debug("body Response I-SCORE = {} ",
					response != null ? response.getGetXMLResWithPDFResult() : "NONE");
			if (response.getGetXMLResWithPDFResult() != null) {
				// setting response DATA
				screeningDTO.setXmlResponse(response.getGetXMLResWithPDFResult().getGetXMLReport());
				// setting byte report
				byte[] encoded = java.util.Base64.getEncoder()
						.encode(response.getGetXMLResWithPDFResult().getGetPDFStream());
				screeningDTO.setIscoreReport(java.util.Base64.getDecoder().decode(encoded));
			}
			else {
				screeningDTO.setXmlResponse(ThirdPartyStatus.ERROR.name());
			}

			DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			InputSource src = new InputSource();
			src.setCharacterStream(
					new StringReader(response.getGetXMLResWithPDFResult().getGetXMLReport()));

			Document doc = builder.parse(src);
			String score = doc.getElementsByTagName("SCORE").item(0).getTextContent();
			String active_loan = doc.getElementsByTagName("ACTIVE_LOANS").item(0).getTextContent();

			NodeList list_num_max_due = doc.getElementsByTagName("MAX_NUM_DAYS_DUE");
			String num_max_due = list_num_max_due.item(0).getTextContent();
			for (int i = 0; i < list_num_max_due.getLength(); i++) {
				if (Integer.parseInt(num_max_due) < Integer
						.parseInt(list_num_max_due.item(i).getTextContent())) {
					num_max_due = list_num_max_due.item(i).getTextContent();
				}
			}

			logger.info("### Send I-SCORE Score = [{}] ### ", score);
			logger.info("### Send I-SCORE ACTIVE LOAN = [{}] ### ", active_loan);
			logger.info("### Send I-SCORE NUM MAX DUE = [{}] ### ", num_max_due);
			screeningDTO.setScore(score);
			screeningDTO.setActiveLoan(active_loan);
			screeningDTO.setMaxNumDaysDue(num_max_due);

		}
		catch (Exception e) {
			logger.error("{}", e.getMessage());
			e.printStackTrace();
		}
		logger.info("Execution method : requestSOAPIScoreAndGenerateIScoreReport() :: DONE");
		return screeningDTO;
	}
}
