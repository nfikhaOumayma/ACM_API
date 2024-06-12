/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingStatutWorkflowRepository;
import com.acm.service.AcmEnvironnementService;
import com.acm.service.SettingGurantorCollateralService;
import com.acm.service.SettingLevelProcessService;
import com.acm.service.SettingRequiredStepService;
import com.acm.service.SettingStatutWorkflowService;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;
import com.acm.utils.models.QSettingStatutWorkflow;
import com.acm.utils.models.SettingStatutWorkflow;
import com.acm.utils.validation.ACMValidationUtils;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingStatutWorkflowServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
@Service
public class SettingStatutWorkflowServiceImpl implements SettingStatutWorkflowService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingStatutWorkflowServiceImpl.class);

	/** The settingStatutWorkflow repository. */
	@Autowired
	private SettingStatutWorkflowRepository settingStatutWorkflowRepository;

	/** The setting gurantor collateral service. */
	@Autowired
	private SettingGurantorCollateralService settingGurantorCollateralService;

	/** The SettingLevelProcess service. */
	@Autowired
	private SettingLevelProcessService settingLevelProcessService;

	/** The SettingRequiredStep service. */
	@Autowired
	private SettingRequiredStepService settingRequiredStepService;

	/** The acm environnement service. */
	@Autowired
	private AcmEnvironnementService acmEnvironnementService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingStatutWorkflowService#find(com.acm.utils.dtos.
	 * SettingStatutWorkflowDTO)
	 */
	@Override
	public List<SettingStatutWorkflowDTO> find(SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		// init QSettingStatutWorkflow
		QSettingStatutWorkflow qSettingStatutWorkflow =
				QSettingStatutWorkflow.settingStatutWorkflow;

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// only enabled row
		if (!Boolean.TRUE.equals(settingStatutWorkflowDTO.getIsNewLoan())) {
			predicate.and(qSettingStatutWorkflow.enabled.eq(Boolean.TRUE));
		}
		// find by client (in case we have multiple client)
		// predicate.and(qSettingStatutWorkflow.client.eq(CommonConstants.APP_CLIENT))

		// check configuration
		if (!ACMValidationUtils.isNullOrEmpty(settingStatutWorkflowDTO.getLoanDTO())) {
			// find by BPMN process
			predicate.and(qSettingStatutWorkflow.processName
					.eq(settingStatutWorkflowDTO.getLoanDTO().getProcessName()));

			List<Integer> excludedCode = new ArrayList<>();
			// check Guarantor/Collateral configuration
			excludedCode.addAll(checkSettingGurantorCollateral(settingStatutWorkflowDTO));
			// check Approvel Level configuration (If topuped loan generate all approval Level
			//if (settingStatutWorkflowDTO.getLoanDTO().getLoanApplicationStatus()
			//		.equals(CommonConstants.NEW_APPLICATION)) {
			excludedCode.addAll(checkSettingApprovelLevel(settingStatutWorkflowDTO));
			//}
			// check SETTING_REQUIRED_STEP
			excludedCode.addAll(checkSettingRequiredSteps(settingStatutWorkflowDTO));

			// find by excluded Code Statut Workflow
			if (!ACMValidationUtils.isNullOrEmpty(excludedCode)) {
				predicate.and(qSettingStatutWorkflow.code.notIn(excludedCode));
			}

			// QueryDSL using springDATA
			Iterable<SettingStatutWorkflow> iterable = settingStatutWorkflowRepository
					.findAll(predicate, Sort.by(Direction.ASC, "orderEtapeProcess"));

			List<SettingStatutWorkflow> settingStatutWorkflows = new ArrayList<>();
			iterable.forEach(settingStatutWorkflows::add);

			// mapping returned list
			List<SettingStatutWorkflowDTO> settingStatutWorkflowDTOs = new ArrayList<>();
			settingStatutWorkflows.forEach(settingStatutWorkflow -> settingStatutWorkflowDTOs
					.add(mapper.map(settingStatutWorkflow, SettingStatutWorkflowDTO.class)));

			logger.info("Returning founded data");
			return settingStatutWorkflowDTOs;
		}
		return new ArrayList<>();
	}

	/**
	 * Check setting gurantor collateral.
	 * 
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	private List<Integer> checkSettingGurantorCollateral(
			SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		List<Integer> excludedSettingGurantorCollateralCode = new ArrayList<>();
		// find config by product ID & mandatory column is TRUE
		List<SettingGurantorCollateralDTO> settingGurantorCollateralDTOs =
				settingGurantorCollateralService.find(new SettingGurantorCollateralDTO(
						settingStatutWorkflowDTO.getLoanDTO().getProductId(), Boolean.TRUE));
		if (!ACMValidationUtils.isNullOrEmpty(settingGurantorCollateralDTOs)) {
			List<String> settings = new ArrayList<>();
			// filter list code GUARANTOR / COLLATERAL
			settingGurantorCollateralDTOs.stream().forEach(
					settingGurantorCollateral -> settings.add(settingGurantorCollateral.getCode()));

			// exclude "GUARANTOR" from list workflow process
			if (!settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR)) {
				excludedSettingGurantorCollateralCode.add(3);
			}
			// exclude "COLLATERAL" from list workflow process
			if (!settings.contains(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_COLLATERAL)) {
				excludedSettingGurantorCollateralCode.add(4);
			}
		}
		else {
			// setting Gurantor Collateral not found
			excludedSettingGurantorCollateralCode.add(3);
			excludedSettingGurantorCollateralCode.add(4);
		}
		return excludedSettingGurantorCollateralCode;
	}

	/**
	 * Check setting approvel level.
	 * 
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	private List<Integer> checkSettingApprovelLevel(
			SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		List<Integer> excludedSettingLevelProcessCode = new ArrayList<>();
		// check ApprobationLevel configuration

		List<SettingLevelProcessDTO> settingLevelProcessDTOs =
				settingLevelProcessService.find(new SettingLevelProcessDTO(
						Long.valueOf(settingStatutWorkflowDTO.getLoanDTO().getProductId()),
						settingStatutWorkflowDTO.getLoanDTO().getApprovelAmount()));

		if (!ACMValidationUtils.isNullOrEmpty(settingLevelProcessDTOs)) {
			List<String> settings = new ArrayList<>();
			// filter list code
			settingLevelProcessDTOs.stream().forEach(settingLevelProcess -> settings
					.add(settingLevelProcess.getSettingLevelDTO().getCode()));

			// exclude "APPROVAL_L1" from list workflow process
			if (!settings.contains("APPROVAL_L1")) {
				excludedSettingLevelProcessCode.add(7);
			}
			// exclude "APPROVAL_L2" from list workflow process
			if (!settings.contains("APPROVAL_L2")) {
				excludedSettingLevelProcessCode.add(8);
			}
			// exclude "APPROVAL_L3" from list workflow process
			if (!settings.contains("APPROVAL_L3")) {
				excludedSettingLevelProcessCode.add(9);
			}
			// exclude "APPROVAL_L4" from list workflow process
			if (!settings.contains("APPROVAL_L4")) {
				excludedSettingLevelProcessCode.add(10);
			}
		}
		else {
			// Setting Level Process Code not found
			excludedSettingLevelProcessCode.add(7);
			excludedSettingLevelProcessCode.add(8);
			excludedSettingLevelProcessCode.add(9);
			excludedSettingLevelProcessCode.add(10);
			logger.warn("CHECK CONFIG IN TABLE : [ACM_SETTING_LEVEL_PROCESS]");
		}
		return excludedSettingLevelProcessCode;
	}

	/**
	 * Check setting required steps.
	 * 
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	private List<Integer> checkSettingRequiredSteps(
			SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		List<Integer> excludedRequiredStepsCode = new ArrayList<>();
		// find config by product ID & mandatory column is TRUE
		List<SettingRequiredStepDTO> settingRequiredStepDTOs =
				settingRequiredStepService.find(new SettingRequiredStepDTO(
						settingStatutWorkflowDTO.getLoanDTO().getProductId(), Boolean.TRUE));

		if (!ACMValidationUtils.isNullOrEmpty(settingRequiredStepDTOs)) {
			List<String> settingsCode = new ArrayList<>();
			// filter list code REQUIRED STEP
			settingRequiredStepDTOs.stream()
					.forEach(setting -> settingsCode.add(setting.getCode()));

			// exclude "STEP_FIELD_VISIT" from list workflow process
			if (!settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_FIELD_VISIT)) {
				excludedRequiredStepsCode.add(2);
			}
			// exclude "STEP_AUDIT_REVIEW" from list workflow process
			if (!settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_AUDIT_REVIEW)) {
				excludedRequiredStepsCode.add(19);
			}
			// exclude "STEP_RISK_REVIEW" from list workflow process
			if (!settingsCode.contains(CommonConstants.ACM_SETTING_REQUIRED_STEP_RISK_REVIEW)) {
				excludedRequiredStepsCode.add(20);
			}
			else {
				try {
					// loading config "RISK_AMOUNT_VALIDATION"
					AcmEnvironnementDTO environnementDTO =
							acmEnvironnementService.find("RISK_AMOUNT_VALIDATION");
					logger.debug("{}", environnementDTO);
					if (!ACMValidationUtils.isNullOrEmpty(environnementDTO)) {
						// getting configured risk amount
						BigDecimal riskAmount = new BigDecimal(environnementDTO.getValue());
						// check next step based on loan amount
						if (settingStatutWorkflowDTO.getLoanDTO().getApplyAmountTotal()
								.compareTo(riskAmount) < 0) {
							excludedRequiredStepsCode.add(20);
						}
					}
				}
				catch (ResourcesNotFoundException e) {
					logger.error("Error while finding setting by key = RISK_AMOUNT_VALIDATION");
					logger.error("{}", e.getMessage());
				}
			}
		}
		else {
			// No setting REQUIRED STEP config was founded
			excludedRequiredStepsCode.add(2);
			excludedRequiredStepsCode.add(19);
			excludedRequiredStepsCode.add(20);
		}
		return excludedRequiredStepsCode;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingStatutWorkflowService#findAll(com.acm.utils.dtos.
	 * SettingStatutWorkflowDTO)
	 */
	@Override
	public List<SettingStatutWorkflowDTO> findAll(
			SettingStatutWorkflowDTO settingStatutWorkflowDTO) {

		// mapping returned list
		List<SettingStatutWorkflowDTO> settingStatutWorkflowDTOs = new ArrayList<>();
		settingStatutWorkflowRepository.findAll()
				.forEach(settingStatutWorkflow -> settingStatutWorkflowDTOs
						.add(mapper.map(settingStatutWorkflow, SettingStatutWorkflowDTO.class)));
		return settingStatutWorkflowDTOs;
	}
}
