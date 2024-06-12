/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.audit;

import java.lang.reflect.Field;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.persistence.PostPersist;
import javax.persistence.PostRemove;
import javax.persistence.PostUpdate;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.acm.constants.common.CommonFunctions;
import com.acm.utils.models.AcmHistory;
import com.acm.utils.models.Customer;
import com.acm.utils.models.UserDefinedFieldGroup;
import com.acm.utils.models.UserDefinedFields;
import com.acm.utils.repository.AcmHistoryRepository;
import com.fasterxml.jackson.core.JsonProcessingException;

/**
 * The listener interface for receiving auditTrail events. The class that is interested in
 * processing a auditTrail event implements this interface, and the object created with that class
 * is registered with a component using the component's addAuditTrailListener method. When the
 * auditTrail event occurs, that object's appropriate method is invoked.
 *
 * @see AuditTrailEvent
 */
@Component
public class AuditTrailListener {

	/** The log. */
	private static final Logger logger = LoggerFactory.getLogger(AuditTrailListener.class);

	/** The parametrage client. */

	static private AcmHistoryRepository acmHistoryRepository;

	/**
	 * Instantiates a new audit trail listener.
	 */
	public AuditTrailListener() {

		super();
	}

	/**
	 * Inits the.
	 *
	 * @param historyRepository the history repository
	 */
	@Autowired
	public void init(AcmHistoryRepository historyRepository) {

		AuditTrailListener.acmHistoryRepository = historyRepository;
	}

	/**
	 * After any persist.
	 *
	 * @param object the object
	 * @throws NoSuchFieldException the no such field exception
	 * @throws SecurityException the security exception
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostPersist
	private void afterAnyPersist(Object object)
			throws NoSuchFieldException, SecurityException, JsonProcessingException {

		saveHistory(object, "CREATE");

	}

	/**
	 * After any update.
	 *
	 * @param object the object
	 * @throws NoSuchFieldException the no such field exception
	 * @throws SecurityException the security exception
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostUpdate
	private void afterAnyUpdate(Object object)
			throws NoSuchFieldException, SecurityException, JsonProcessingException {

		saveHistory(object, "UPDATE");
	}

	/**
	 * After any remove.
	 *
	 * @param object the object
	 * @throws NoSuchFieldException the no such field exception
	 * @throws SecurityException the security exception
	 * @throws JsonProcessingException the json processing exception
	 */
	@PostRemove
	private void afterAnyRemove(Object object)
			throws NoSuchFieldException, SecurityException, JsonProcessingException {

		saveHistory(object, "REMOVE");
	}

	/**
	 * Save history.
	 *
	 * @param object the object
	 * @param typeAction the type action
	 * @throws NoSuchFieldException the no such field exception
	 * @throws SecurityException the security exception
	 */
	private void saveHistory(Object object, String typeAction)
			throws NoSuchFieldException, SecurityException {

		AcmHistory acmHistory = new AcmHistory();

		acmHistory.setActions(typeAction);
		acmHistory.setDateAction(new Date());

		acmHistory.setActionBy(CommonFunctions.getConnectedUser(logger).getFullName());
		acmHistory.setTypeObject(object.getClass().getSimpleName());

		try {
			Map<String, Object> objectMap = new HashMap<String, Object>();

			Field[] allFields = object.getClass().getDeclaredFields();
			boolean exist = false;
			for (Field field : allFields) {
				field.setAccessible(true);
				Class<?> type = field.getType();

				if (!java.util.Set.class.isAssignableFrom(type)
						&& !UserDefinedFields.class.isAssignableFrom(type)
						&& !Customer.class.isAssignableFrom(type)
						&& !UserDefinedFieldGroup.class.isAssignableFrom(type)) {
					Object value = field.get(object);
					if (!field.getName().equals("photo")) {
						objectMap.put(field.getName(), value);
					}
					if (!exist && (field.getName().equals("id") || field.getName().equals("idLoan")
							|| field.getName().equals("idMezaCard"))) {
						String stringToConvert = String.valueOf(value);
						Long convertedLong = Long.parseLong(stringToConvert);
						acmHistory.setIdObject(convertedLong);
						exist = true;
					}
				}

			}

			acmHistory.setValueObject(CommonFunctions.convertObjectToJSONString(objectMap));
			acmHistoryRepository.save(acmHistory);
			logger.info("[" + object.getClass().getSimpleName() + "] About to" + typeAction + "a"
					+ object.getClass().getSimpleName());
		}
		catch (Exception e) {
			e.printStackTrace();
		}

		

	}
}
