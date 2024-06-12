/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.soap.kyc.model;

import javax.xml.bind.annotation.XmlRegistry;

/**
 * This object contains factory methods for each Java content interface and Java element interface
 * generated in the com.acm.soap.kyc.model package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the Java representation
 * for XML content. The Java representation of XML content can consist of schema derived interfaces
 * and classes representing the binding of schema type definitions, element declarations and model
 * groups. Factory methods for each of these are provided in this class.
 */
@XmlRegistry
public class ObjectFactory {

	/**
	 * Create a new ObjectFactory that can be used to create new instances of schema derived classes
	 * for package: com.acm.soap.kyc.model.
	 */
	public ObjectFactory() {

	}

	/**
	 * Create an instance of {@link GetPersonDetails }.
	 *
	 * @return the gets the person details
	 */
	public GetPersonDetails createGetPersonDetails() {

		return new GetPersonDetails();
	}

	/**
	 * Create an instance of {@link GetPersonDetailsResponse }.
	 *
	 * @return the gets the person details response
	 */
	public GetPersonDetailsResponse createGetPersonDetailsResponse() {

		return new GetPersonDetailsResponse();
	}

	/**
	 * Create an instance of {@link PersonDetails }.
	 *
	 * @return the person details
	 */
	public PersonDetails createPersonDetails() {

		return new PersonDetails();
	}

	/**
	 * Create an instance of {@link GetCompanyInformation }.
	 *
	 * @return the gets the company information
	 */
	public GetCompanyInformation createGetCompanyInformation() {

		return new GetCompanyInformation();
	}

	/**
	 * Create an instance of {@link CompanyInformationRequest }.
	 *
	 * @return the company information request
	 */
	public CompanyInformationRequest createCompanyInformationRequest() {

		return new CompanyInformationRequest();
	}

	/**
	 * Create an instance of {@link GetCompanyInformationResponse }.
	 *
	 * @return the gets the company information response
	 */
	public GetCompanyInformationResponse createGetCompanyInformationResponse() {

		return new GetCompanyInformationResponse();
	}

	/**
	 * Create an instance of {@link CompanyInformationResponse }.
	 *
	 * @return the company information response
	 */
	public CompanyInformationResponse createCompanyInformationResponse() {

		return new CompanyInformationResponse();
	}

	/**
	 * Create an instance of {@link GetCompanyDocument }.
	 *
	 * @return the gets the company document
	 */
	public GetCompanyDocument createGetCompanyDocument() {

		return new GetCompanyDocument();
	}

	/**
	 * Create an instance of {@link CompanyDocumentRequest }.
	 *
	 * @return the company document request
	 */
	public CompanyDocumentRequest createCompanyDocumentRequest() {

		return new CompanyDocumentRequest();
	}

	/**
	 * Create an instance of {@link GetCompanyDocumentResponse }.
	 *
	 * @return the gets the company document response
	 */
	public GetCompanyDocumentResponse createGetCompanyDocumentResponse() {

		return new GetCompanyDocumentResponse();
	}

	/**
	 * Create an instance of {@link CompanyDocumentResponse }.
	 *
	 * @return the company document response
	 */
	public CompanyDocumentResponse createCompanyDocumentResponse() {

		return new CompanyDocumentResponse();
	}

	/**
	 * Create an instance of {@link Query }.
	 *
	 * @return the query
	 */
	public Query createQuery() {

		return new Query();
	}

	/**
	 * Create an instance of {@link Company }.
	 *
	 * @return the company
	 */
	public Company createCompany() {

		return new Company();
	}

	/**
	 * Create an instance of {@link ArrayOfCompany }.
	 *
	 * @return the array of company
	 */
	public ArrayOfCompany createArrayOfCompany() {

		return new ArrayOfCompany();
	}

}
