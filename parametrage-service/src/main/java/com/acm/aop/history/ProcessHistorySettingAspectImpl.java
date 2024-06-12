/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.aop.history;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.SettingHistoriqueService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingHistoriqueDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.AcmEnvironnement;
import com.acm.utils.models.Groupe;
import com.acm.utils.models.Habilitation;
import com.acm.utils.models.SettingDocumentProduct;
import com.acm.utils.models.SettingDocumentType;
import com.acm.utils.models.SettingGurantorCollateral;
import com.acm.utils.models.SettingLevel;
import com.acm.utils.models.SettingLevelProcess;
import com.acm.utils.models.SettingMotifRejets;
import com.acm.utils.models.SettingNotifications;

/**
 * {@link ProcessHistorySettingAspectImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.2
 */
@Aspect
@Configuration
public class ProcessHistorySettingAspectImpl {

	/** The logger. */
	private Logger logger = LoggerFactory.getLogger(this.getClass());

	/** The setting historique service. */
	@Autowired
	private SettingHistoriqueService settingHistoriqueService;

	/** The user client. */
	@Autowired(required = false)
	private UserClient userClient;

	/**
	 * Methods annoatated with history loan.
	 */
	@Pointcut(value = "@annotation(com.acm.aop.history.ProcessHistorySetting)")
	public void methodsAnnoatatedWithHistorySetting() {

		// defines pointcut for methods annotated with @ProcessHistorySetting
	}

	/**
	 * After execution.
	 *
	 * @param joinPoint the join point
	 * @param result the result
	 * @param action the action
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@AfterReturning(value = "methodsAnnoatatedWithHistorySetting()  && @annotation(action)",
			returning = "result")
	public void afterExecution(JoinPoint joinPoint, Object result, ProcessHistorySetting action)
			throws ResourcesNotFoundException {

		logger.debug("Init History Setting AOP for {} Method", joinPoint.getSignature());
		// find connected user
		UserDTO connectedUser = CommonFunctions.getConnectedUser(logger);
		// process history by action
		mappingAndSavingHistory(result, connectedUser, action.action());
		logger.debug("SETTING HISTORY SAVED :: DONE ");
	}

	/**
	 * Mapping and saving history for new Entry.
	 * 
	 * @author HaythemBenizid
	 * @param result the result
	 * @param connectedUser the connected user
	 * @param action the action
	 */
	private void mappingAndSavingHistory(Object result, UserDTO connectedUser, String action) {

		if (result instanceof SettingGurantorCollateralDTO) {
			SettingGurantorCollateralDTO castObject = (SettingGurantorCollateralDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingGurantorCollateral.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingLevelDTO) {
			SettingLevelDTO castObject = (SettingLevelDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingLevel.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingLevelProcessDTO) {
			SettingLevelProcessDTO castObject = (SettingLevelProcessDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingLevelProcess.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingDocumentProductDTO) {
			SettingDocumentProductDTO castObject = (SettingDocumentProductDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingDocumentProduct.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingDocumentTypeDTO) {
			SettingDocumentTypeDTO castObject = (SettingDocumentTypeDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingDocumentType.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingMotifRejetsDTO) {
			SettingMotifRejetsDTO castObject = (SettingMotifRejetsDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingMotifRejets.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof SettingNotificationsDTO) {
			SettingNotificationsDTO castObject = (SettingNotificationsDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(SettingNotifications.class), action,
					castObject.getIdSettingNotification(),
					CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof HabilitationDTO) {
			HabilitationDTO castObject = (HabilitationDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(Habilitation.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof GroupeDTO) {
			GroupeDTO castObject = (GroupeDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(Groupe.class), action, castObject.getId(),
					CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
		else if (result instanceof AcmEnvironnementDTO) {
			AcmEnvironnementDTO castObject = (AcmEnvironnementDTO) result;
			settingHistoriqueService.save(new SettingHistoriqueDTO(
					CommonFunctions.getTableNameFromClass(AcmEnvironnement.class), action,
					castObject.getId(), CommonFunctions.convertObjectToJSONString(castObject),
					connectedUser.getFullName()));
		}
	}
}
