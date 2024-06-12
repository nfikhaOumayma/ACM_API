/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.kyc.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;
import org.springframework.stereotype.Service;
import org.springframework.ws.client.core.WebServiceTemplate;
import org.springframework.ws.soap.client.core.SoapActionCallback;

import com.acm.client.ParametrageClient;
import com.acm.soap.kyc.model.GetPersonDetails;
import com.acm.soap.kyc.model.GetPersonDetailsResponse;
import com.acm.soap.kyc.model.KycDTO;
import com.acm.soap.kyc.model.ObjectFactory;
import com.acm.soap.kyc.service.KycSoapService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.validation.ACMValidationUtils;

/**
 * {@link KycSoapServiceImpl} class.
 *
 * @author yesser.somai
 * @since 1.0.8
 */
@Service
public class KycSoapServiceImpl implements KycSoapService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(KycSoapServiceImpl.class);

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The message source. */
	@Autowired
	private MessageSource messageSource;

	/** The jaxb 2 marshaller. */
	@Autowired
	private Jaxb2Marshaller jaxb2Marshaller;

	/** The web service template. */
	private WebServiceTemplate webServiceTemplate;

	/*
	 * (non-Javadoc)
	 * @see com.acm.soap.kyc.service.KycSoapService#requestSOAPKyc()
	 */
	@Override
	public KycDTO requestSOAPKycGetPersonDetails(KycDTO kycDTO) {

		List<AcmEnvironnementDTO> environnementDTOs =
				parametrageClient.findLikeKey(new AcmEnvironnementDTO("TAMKEEN_SOAP_KYC_REQUEST"));
		String strUserID = "";
		String strPassword = "";
		String pathServiceSOAP = "";
		String referenceNumber = "";

		// getting configuration
		for (AcmEnvironnementDTO acmEnvironnementDTO : environnementDTOs) {
			if (Boolean.TRUE.equals(
					acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_KYC_REQUEST_USER"))) {
				// mf0018000100013ab
				strUserID = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE.equals(
					acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_KYC_REQUEST_PASS"))) {
				// Tamkeen1234
				strPassword = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE.equals(
					acmEnvironnementDTO.getKey().contains("TAMKEEN_SOAP_KYC_REQUEST_PATH"))) {
				pathServiceSOAP = acmEnvironnementDTO.getValue();
			}
			if (Boolean.TRUE.equals(acmEnvironnementDTO.getKey()
					.contains("TAMKEEN_SOAP_KYC_REQUEST_REFERENCE_NUMBER"))) {
				referenceNumber = acmEnvironnementDTO.getValue();
			}
		}

		// init params
		// Check if all configuration exist
		if (!ACMValidationUtils.isNullOrEmpty(strUserID)
				&& !ACMValidationUtils.isNullOrEmpty(strPassword)
				&& !ACMValidationUtils.isNullOrEmpty(pathServiceSOAP)
				&& !ACMValidationUtils.isNullOrEmpty(referenceNumber)) {

			// init body request params
			// TODO
			String[] params = {strUserID, strPassword, referenceNumber, kycDTO.getNationalID()};

			// mapping data
			String bodyRequest = messageSource.getMessage("kyc.soap.body.request", params,
					LocaleContextHolder.getLocale());
			logger.info("bodyRequest KYC = {} ", bodyRequest);
			kycDTO.setXmlRequest(bodyRequest);
		}

		ObjectFactory objectFactory = new ObjectFactory();
		GetPersonDetails personDetails = objectFactory.createGetPersonDetails();

		// Soap User Name
		personDetails.setUserCode(strUserID);
		// Soap Password
		personDetails.setPassword(strPassword);
		// Reference Number
		personDetails.setReferenceNumber(referenceNumber);
		// National ID Number
		personDetails.setNationalIDNumber(kycDTO.getNationalID());

		logger.info("### Send KYC Request using USER = [{}] && PASS = [{}] ### ", strUserID,
				strPassword);
		// call SOAP Service
		webServiceTemplate = new WebServiceTemplate(jaxb2Marshaller);
		String soapAction =
				"https://nid.i-score.com.eg/Iscore_WebService/services/NationalID_InquiryService";
		logger.info("### SOAP_ACTION= {}", soapAction);
		logger.info("### PATH= {}", pathServiceSOAP);
		SoapActionCallback soapActionCallback = new SoapActionCallback(soapAction);
		GetPersonDetailsResponse response = new GetPersonDetailsResponse();
		try {
			// return data
			response = (GetPersonDetailsResponse) webServiceTemplate
					.marshalSendAndReceive(pathServiceSOAP, personDetails, soapActionCallback);
			logger.debug("body Response KYC = {} ",
					response != null ? response.getGetPersonDetailsReturn() : "NONE");
		}
		catch (Exception e) {
			logger.error("### Error has been occurred while call SOAP API= {}", e.getMessage());
			e.printStackTrace();
			// returning ERROR
			response = new GetPersonDetailsResponse();
			response.getGetPersonDetailsReturn().setErrorCode(e.hashCode());
			kycDTO.setXmlResponse(response.getGetPersonDetailsReturn());

		}

		kycDTO.setXmlResponse(response.getGetPersonDetailsReturn());
		return kycDTO;
	}

}
