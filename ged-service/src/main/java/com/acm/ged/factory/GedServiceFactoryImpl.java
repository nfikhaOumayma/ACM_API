/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.ged.factory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.chemistry.opencmis.client.api.Document;
import org.apache.chemistry.opencmis.client.api.Folder;
import org.apache.chemistry.opencmis.client.api.ItemIterable;
import org.apache.chemistry.opencmis.client.api.QueryResult;
import org.apache.chemistry.opencmis.client.api.Repository;
import org.apache.chemistry.opencmis.client.api.Session;
import org.apache.chemistry.opencmis.client.api.SessionFactory;
import org.apache.chemistry.opencmis.client.runtime.SessionFactoryImpl;
import org.apache.chemistry.opencmis.commons.PropertyIds;
import org.apache.chemistry.opencmis.commons.SessionParameter;
import org.apache.chemistry.opencmis.commons.enums.BindingType;
import org.apache.chemistry.opencmis.commons.exceptions.CmisConnectionException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.impl.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.acm.config.ged.GedConfigProperties;
import com.acm.constants.common.CommonConstantGED;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;

/**
 * {@link GedServiceFactoryImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.5.0
 */
@Service
public class GedServiceFactoryImpl implements GedServiceFactory {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(GedServiceFactoryImpl.class);
	@Autowired
	GedConfigProperties gedConfigProperties;

	/*
	 * (non-Javadoc)
	 * @see com.acm.ged.factory.GedServiceFactory#createSession()
	 */
	@Override
	public Session createSession() {

		try {
			Map<String, String> parameter = new HashMap<>();
			// user credentials
			parameter.put(SessionParameter.USER, gedConfigProperties.getLogin());
			parameter.put(SessionParameter.PASSWORD, gedConfigProperties.getPassword());
			// connection settings
			parameter.put(SessionParameter.ATOMPUB_URL,
					gedConfigProperties.getGedConfig(CommonConstantGED.ATOMPUB_GED_URL));
			parameter.put(SessionParameter.BINDING_TYPE, BindingType.ATOMPUB.value());
			// create SESSION
			SessionFactory factory = SessionFactoryImpl.newInstance();
			List<Repository> repositories = factory.getRepositories(parameter);
			if (ACMValidationUtils.isNullOrEmpty(repositories)) {
				throw new CmisConnectionException(
						"Failed to connect to the Alfresco Server, No repository found");
			}
			Repository repository = repositories.get(0);
			return repository.createSession();
		}
		catch (Exception e) {
			throw new CmisConnectionException("Failed to connect to the GED Server");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.ged.factory.GedServiceFactory#findGedSiteToken()
	 */
	@Override
	public String findGedSiteToken() {

		try {
			HttpHeaders headers = new HttpHeaders();
			headers.set(CommonConstantGED.CONTENT_TYPE, CommonConstantGED.APPLICATION_JSON);
			RestTemplate restTemplate = new RestTemplate();
			JSONObject json = new JSONObject();
			json.put(CommonConstantGED.USER_ID, gedConfigProperties.getLogin());
			json.put(CommonConstantGED.TOKEN_PASS, gedConfigProperties.getPassword());
			HttpEntity<JSONObject> entity = new HttpEntity<>(json, headers);
			Map<?, ?> tokenObject =
					(Map<?, ?>) restTemplate
							.postForObject(gedConfigProperties.getGedConfig(
									CommonConstantGED.URI_GED_TOKEN), entity, Map.class)
							.get(CommonConstantGED.ENTRY);
			logger.debug("returning Ged Site Token :: DONE");
			return CommonFunctions.encodeBase64((tokenObject.get(CommonConstantGED.ID)).toString());
		}
		catch (Exception e) {
			throw new CmisConnectionException(
					"Failed to generate TOKEN to connect to the GED Server");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.ged.factory.GedServiceFactory#findSite(org.apache.chemistry.opencmis.client.api.
	 * Session)
	 */
	@Override
	public Folder findSite(Session session) {

		logger.debug("Find Site");
		Preconditions.checkNotNull(session, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		if (ACMValidationUtils.isNullOrEmpty(CommonConstantGED.ACM_SITE)) {
			return null;
		}
		Folder site = null;
		String query =
				String.format(CommonConstantGED.QUERY_FIND_DOCUMENT, CommonConstantGED.ACM_SITE);
		logger.debug("query = {}", query);
		ItemIterable<QueryResult> result = session.query(query, false);
		for (QueryResult queryResult : result) {
			site = (Folder) session.getObject(queryResult
					.getPropertyById(CommonConstantGED.CMIS_OBJECT_ID).getFirstValue().toString());
			if (site != null) {
				break;
			}
		}
		logger.debug("returning GED Site :: DONE");
		return site;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.ged.factory.GedServiceFactory#addTag(org.apache.chemistry.opencmis.client.api.
	 * Document, java.lang.String, java.lang.String)
	 */
	@Override
	@Async
	public void addTag(Document document, String tag, String token) {

		Preconditions.checkNotNull(document, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(tag, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(token, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Preconditions.checkNotNull(document.getId(),
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		try {
			String uri = gedConfigProperties.getGedConfig(CommonConstantGED.URI_GED_NODES)
					+ document.getId() + "/tags";
			HttpHeaders headers = new HttpHeaders();
			headers.set(CommonConstantGED.CONTENT_TYPE, CommonConstantGED.APPLICATION_JSON);
			headers.set(CommonConstantGED.AUTHORIZATION, CommonConstantGED.BASIC + token);
			RestTemplate restTemplate = new RestTemplate();
			JSONObject json = new JSONObject();
			json.put("tag", tag);
			HttpEntity<JSONObject> entity = new HttpEntity<>(json, headers);
			restTemplate.postForObject(uri, entity, String.class);

			logger.debug("Add tag = {} in GED for document with Name = {} :: DONE", tag,
					document.getName());
		}
		catch (Exception e) {
			logger.error("Error : Failed to add tag : {} ", tag);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see
	 * com.acm.ged.factory.GedServiceFactory#createOrLoadFolder(org.apache.chemistry.opencmis.client
	 * .api. Session, java.lang.String, java.lang.String)
	 */
	@Override
	public Folder createOrLoadFolder(Session session, Folder rootFolder, String folderName) {

		Folder subFolder = null;
		logger.debug("folderName : {} ", folderName);
		try {
			subFolder = (Folder) session.getObjectByPath(
					rootFolder.getPath() + CommonConstantGED.SLASH_PATH + folderName);
			logger.debug("Folder {} already existed!", folderName);
		}
		catch (CmisObjectNotFoundException e) {
			logger.warn("try to create Folder in GED!");
			try {
				// prepare properties
				Map<String, Object> properties = new HashMap<>();
				properties.put(PropertyIds.NAME, folderName);
				properties.put(PropertyIds.OBJECT_TYPE_ID, "cmis:folder");
				// create new folder in GED
				subFolder = rootFolder.createFolder(properties);
				logger.debug("Created new folder: {}", subFolder.getId());
			}
			catch (Exception ex) {
				logger.error("Error while creating new file: {}", e.getMessage());
				// try to load the folder if exist
				subFolder = (Folder) session.getObjectByPath(
						rootFolder.getPath() + CommonConstantGED.SLASH_PATH + folderName);
			}
		}
		return subFolder;
	}
}
