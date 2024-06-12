/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.soap.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;
import org.springframework.stereotype.Service;
import org.springframework.ws.client.core.WebServiceTemplate;
import org.springframework.ws.soap.client.core.SoapActionCallback;

import com.acm.soap.model.GetXMLResWithPDF;
import com.acm.soap.model.GetXMLResWithPDFResponse;
import com.acm.soap.model.PostRequest;
import com.acm.soap.model.PostRequestResponse;

/**
 * {@link SoapClient} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Service
public class SoapClient {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SoapClient.class);

	/** The jaxb 2 marshaller. */
	@Autowired
	private Jaxb2Marshaller jaxb2Marshaller;

	/** The web service template. */
	private WebServiceTemplate webServiceTemplate;

	/**
	 * Gets the post request response.
	 * 
	 * @author HaythemBenizid
	 * @param postRequest the post request
	 * @param path the path
	 * @return the post request response
	 */
	public PostRequestResponse getPostRequestResponse(PostRequest postRequest, String path) {

		webServiceTemplate = new WebServiceTemplate(jaxb2Marshaller);
		String soapAction = "http://tempuri.org/PostRequest";
		logger.info("### SOAP_ACTION= {}", soapAction);
		logger.info("### PATH= {}", path);
		SoapActionCallback soapActionCallback = new SoapActionCallback(soapAction);
		try {
			// return data
			return (PostRequestResponse) webServiceTemplate.marshalSendAndReceive(path, postRequest,
					soapActionCallback);
		}

		catch (Exception e) {
			logger.error("### Error has been occurred while call SOAP API= {}", e.getMessage());
			e.printStackTrace();
			// returning ERROR
			PostRequestResponse postRequestResponse = new PostRequestResponse();
			postRequestResponse.setPostRequestResult("ERROR");
			return postRequestResponse;
		}
	}

	/**
	 * Generate report - Gets the gets the XML res with PDF response.
	 *
	 * @author HaythemBenizid
	 * @param getXMLResWithPDF the get XML res with PDF
	 * @param path the path
	 * @return the gets the XML res with PDF response
	 */
	public GetXMLResWithPDFResponse getGetXMLResWithPDFResponse(GetXMLResWithPDF getXMLResWithPDF,
			String path) {

		webServiceTemplate = new WebServiceTemplate(jaxb2Marshaller);
		String soapAction = "http://tempuri.org/GetXMLResWithPDF";
		logger.info("### SOAP_ACTION= {}", soapAction);
		logger.info("### PATH= {}", path);
		SoapActionCallback soapActionCallback = new SoapActionCallback(soapAction);
		try {
			// return data
			return (GetXMLResWithPDFResponse) webServiceTemplate.marshalSendAndReceive(path,
					getXMLResWithPDF, soapActionCallback);
		}
		catch (Exception e) {
			logger.error("### Error has been occured while call SOAP API= {}", e.getMessage());
			e.printStackTrace();
			// returning NULL
			return null;
		}
	}
}
