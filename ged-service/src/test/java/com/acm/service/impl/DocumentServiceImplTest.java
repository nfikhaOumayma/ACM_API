/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.BDDMockito.given;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Repository;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.assertj.core.api.WithAssertions;
import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.web.client.RestTemplate;

import com.acm.client.ParametrageClient;
import com.acm.config.ged.GedConfigProperties;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.GEDException;
import com.acm.ged.factory.GedServiceFactory;
import com.acm.utils.dtos.GedDocumentDTO;

/**
 * {@link DocumentServiceImplTest} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
class DocumentServiceImplTest implements WithAssertions {

	/** The Garantie service. */
	@InjectMocks
	private DocumentServiceImpl documentService;

	/** The user client. */
	@Mock
	private ParametrageClient userClient;

	/** The ged service factory. */
	@Mock
	private GedServiceFactory gedServiceFactory;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Spy
	private Environment env;
	@Autowired
	private GedConfigProperties gedConfigProperties;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Display document.
	 * 
	 * @throws GEDException
	 */
	@Test
	@Disabled
	void displayDocument() throws GEDException {

		given(gedServiceFactory.findGedSiteToken()).willReturn(findGedSiteToken());
		// WHEN
		byte[] files = documentService.displayDocument("1");
		// THEN
		assertThat(files).isNotNull();
		assertThat(files).isNotEmpty();
	}

	/**
	 * Find ged site token.
	 *
	 * @return the string
	 */
	private String findGedSiteToken() {

		HttpHeaders headers = new HttpHeaders();
		headers.set(CommonConstantGED.CONTENT_TYPE, CommonConstantGED.APPLICATION_JSON);
		RestTemplate restTemplate = new RestTemplate();
		JSONObject json = new JSONObject();
		json.put(CommonConstantGED.USER_ID, gedConfigProperties.getLogin());
		json.put(CommonConstantGED.TOKEN_PASS, gedConfigProperties.getPassword());
		HttpEntity<JSONObject> entity = new HttpEntity<>(json, headers);
		Map<?, ?> tokenObject = (Map<?, ?>) restTemplate
				.postForObject(CommonConstantGED.URI_GED_TOKEN, entity, Map.class)
				.get(CommonConstantGED.ENTRY);
		return CommonFunctions.encodeBase64((tokenObject.get(CommonConstantGED.ID)).toString());
	}

	/**
	 * Should find the document by id.
	 *
	 * @throws GEDException the ged exception
	 */
	@Test
	@Disabled
	void shouldFindDocumentById() throws GEDException {

		// GIVEN
		String id = "1";
		given(gedServiceFactory.findGedSiteToken()).willReturn(findGedSiteToken());
		given(gedServiceFactory.createSession()).willReturn(createSession());

		// WHEN
		GedDocumentDTO file = documentService.find(id);
		// THEN
		assertThat(file).isNotNull();
		assertThat(file.getId()).isEqualTo("1");
	}

	/**
	 * Creates the session.
	 *
	 * @return the session
	 */
	private Session createSession() {

		Map<String, String> parameter = new HashMap<>();
		// user credentials
		parameter.put(SessionParameter.USER, gedConfigProperties.getLogin());
		parameter.put(SessionParameter.PASSWORD, gedConfigProperties.getPassword());
		// connection settings
		parameter.put(SessionParameter.ATOMPUB_URL, CommonConstantGED.ATOMPUB_GED_URL);
		parameter.put(SessionParameter.BINDING_TYPE, BindingType.ATOMPUB.value());
		// create SESSION
		SessionFactory factory = SessionFactoryImpl.newInstance();
		List<Repository> repositories = factory.getRepositories(parameter);
		Repository repository = repositories.get(0);
		return repository.createSession();
	}

	/**
	 * Should find the document by id.
	 *
	 * @throws GEDException the ged exception
	 */
	@Test
	@Disabled
	void shouldThrowExceptionWhenFindDocumentById() throws GEDException {

		// GIVEN
		String id = "hdyd-bsd-2444";
		// WHEN
		Executable result = () -> documentService.find(id);
		// THEN
		// THEN
		GEDException exception = assertThrows(GEDException.class, result);
		assertThat(exception).isNotNull();
	}
}
