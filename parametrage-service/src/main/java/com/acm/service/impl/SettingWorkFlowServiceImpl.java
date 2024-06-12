/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.CollectionSettingException;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.repository.PlaningStepRepository;
import com.acm.repository.SettingCollectionRepository;
import com.acm.repository.SettingListValuesRepository;
import com.acm.repository.SettingWorkFlowRepository;
import com.acm.repository.StepRiskSettingRepository;
import com.acm.service.SettingWorkFlowService;
import com.acm.service.WorkflowStepUdfGroupeService;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.dtos.RiskSettingStepDTO;
import com.acm.utils.dtos.SettingListValuesDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;
import com.acm.utils.dtos.WorkflowStepUdfGroupeDTO;
import com.acm.utils.enums.SettingListValuesTable;
import com.acm.utils.models.CollectionStep;
import com.acm.utils.models.PlaningStep;
import com.acm.utils.models.QCollectionStep;
import com.acm.utils.models.QWorkFlowStep;
import com.acm.utils.models.SettingListValues;
import com.acm.utils.models.SettingTypeRisk;
import com.acm.utils.models.StepRiskSetting;
import com.acm.utils.models.WorkFlowStep;
import com.acm.utils.validation.ACMValidationUtils;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class SettingWorkFlowServiceImpl.
 */
@Service
public class SettingWorkFlowServiceImpl implements SettingWorkFlowService {

	/** The setting work flow repository. */
	@Autowired
	private SettingWorkFlowRepository settingWorkFlowRepository;

	/** The setting collection repository. */
	@Autowired
	private SettingCollectionRepository settingCollectionRepository;

	/** The setting list values repository. */
	@Autowired
	SettingListValuesRepository settingListValuesRepository;

	/** The workflow step udf groupe service. */
	@Autowired
	private WorkflowStepUdfGroupeService workflowStepUdfGroupeService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user client. */
	@Autowired
	private UserClient userClient;

	/** The transvers client. */
	@Autowired
	private TransversClient transversClient;

	/** The planing step repository. */
	@Autowired
	PlaningStepRepository planingStepRepository;

	/** The step risk setting repository. */
	@Autowired
	StepRiskSettingRepository stepRiskSettingRepository;

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SettingWorkFlowServiceImpl.class);

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingWorkFlowService#saveApprovalSteps(java.util.List)
	 */
	@Override
	public List<WorkFlowStepDTO> saveSteps(List<WorkFlowStepDTO> workFlowStepDTOs, Long productId,
			String process) throws WorkFlowSettingException {

		logger.info("Start Save Approval Steps Methods");

		Long settingVersion = 0L;
		// check Product ID
		if (!ACMValidationUtils.isNullOrEmpty(productId)
				&& !ACMValidationUtils.isNullOrEmpty(process)) {

			// disable old setting
			BooleanBuilder predicate = new BooleanBuilder();
			QWorkFlowStep qWorkFlowStep = QWorkFlowStep.workFlowStep;
			predicate.and(qWorkFlowStep.enabled.eq(Boolean.TRUE));
			predicate.and(qWorkFlowStep.productId.eq(productId));
			predicate.and(qWorkFlowStep.process.eq(process));
			Iterable<WorkFlowStep> workFlowSteps = settingWorkFlowRepository.findAll(predicate);

			if (!ACMValidationUtils.isNullOrEmpty(workFlowSteps)) {
				settingVersion = workFlowSteps.iterator().next().getProcessVersion();
				workFlowSteps.forEach(step -> {
					step.setEnabled(Boolean.FALSE);
				});
				settingWorkFlowRepository.saveAll(workFlowSteps);

			}

		}
		else {
			logger.error("Error save Steps with Product ID et Process Name NULL ");
			throw new WorkFlowSettingException(
					new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
							CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION,
							new TechnicalException()),
					CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION);
		}

		// check step list empty of null
		if (ACMValidationUtils.isNullOrEmpty(workFlowStepDTOs)) {
			return new ArrayList<>();
		}

		UserDTO userDTO = userClient.find();
		// prepare list of steps for save
		final Long newVersion = settingVersion + 1;
		List<SettingListValues> settingListValues = settingListValuesRepository
				.findByTableAbacusName(SettingListValuesTable.FEES.tableName());
		List<WorkFlowStep> newWorkFlowSteps = new ArrayList<>();
		workFlowStepDTOs.forEach(workFlowStepDTO -> {
			workFlowStepDTO.setProcessVersion(newVersion);
			workFlowStepDTO.setIdWorkFlowStep(null);
			List<Long> listIdFees = new ArrayList<>();
			workFlowStepDTO.getLstFees().forEach(item -> listIdFees.add(item.getCufeeID()));

			// Set<SettingListValuesDTO> ListSettingValuesForWF = new HashSet<>();
			settingListValues.stream()
					.filter(setting -> listIdFees.contains(new Long(setting.getIdExtern())))
					.collect(Collectors.toSet()).forEach(item -> {
						workFlowStepDTO.getLstFeesListValue()
								.add(mapper.map(item, SettingListValuesDTO.class));
						// ListSettingValuesForWF.add());
					});

			WorkFlowStep modelObj = (WorkFlowStep) CommonFunctions.mapperToSave(
					(mapper.map(workFlowStepDTO, WorkFlowStep.class)), userClient, logger, userDTO);
			// save planing
			if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getPlaningDto())) {
				PlaningStep planingStep =
						mapper.map(workFlowStepDTO.getPlaningDto(), PlaningStep.class);
				planingStep.setLstDay(workFlowStepDTO.getPlaningDto().getLstDay().toString());
				modelObj.setPlaningStep(planingStepRepository.save(planingStep));

			}

			WorkFlowStep workFlowStepNew =
					settingWorkFlowRepository.save((WorkFlowStep) CommonFunctions.mapperToSave(
							(mapper.map(workFlowStepDTO, WorkFlowStep.class)), userClient, logger,
							userDTO));
			newWorkFlowSteps.add(workFlowStepNew);
			if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getListRiskSetting())) {
				workFlowStepDTO.getListRiskSetting().forEach(element -> {
					StepRiskSetting stepRiskSetting = new StepRiskSetting();
					stepRiskSetting.setWorkFlowStep(workFlowStepNew);
					stepRiskSetting.setSettingTypeRisk(new SettingTypeRisk());
					stepRiskSetting.getSettingTypeRisk().setId(element.getIdRiskSetting());
					stepRiskSetting.setEditable(element.getIsEditable());
					stepRiskSettingRepository.save(stepRiskSetting);

				});
			}

		});

		// save new settings

		List<WorkFlowStepDTO> newWorkFlowStepDTOs = new ArrayList<>();

		newWorkFlowSteps.forEach(newWorkFlowStep -> newWorkFlowStepDTOs
				.add(mapper.map(newWorkFlowStep, WorkFlowStepDTO.class)));

		List<WorkflowStepUdfGroupeDTO> newWorkFlowStepDTO = new ArrayList<>();
		Collections.sort(workFlowStepDTOs, Comparator.comparing(WorkFlowStepDTO::getOrder));
		Collections.sort(newWorkFlowSteps, Comparator.comparing(WorkFlowStep::getOrder));
		//
		for (int i = 0; i < workFlowStepDTOs.size(); i++) {
			for (WorkflowStepUdfGroupeDTO item : workFlowStepDTOs.get(i)
					.getWorkflowStepUdfGroupe()) {
				item.setIdWorkflowStep(newWorkFlowSteps.get(i).getIdWorkFlowStep());
				newWorkFlowStepDTO.add(item);
			}

		}
		workflowStepUdfGroupeService.saveAll(newWorkFlowStepDTO);
		logger.info("End Save Approval Steps Methods");
		return newWorkFlowStepDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingWorkFlowService#findApprovalStepsByProduct(java.lang.Long)
	 */
	@Override
	public List<WorkFlowStepDTO> findSteps(WorkFlowStepDTO workFlowStepDTO)
			throws WorkFlowSettingException {

		// check step list empty of null
		if (ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getProductId())
				&& ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getEnabled())
				&& ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getProcess())
				&& ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getIdWorkFlowStep())) {
			logger.error("Error find steps with productId equal NULL");
			throw new WorkFlowSettingException(
					new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
							CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION,
							new TechnicalException()),
					CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION);
		}

		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();
		QWorkFlowStep qWorkFlowStep = QWorkFlowStep.workFlowStep;

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getEnabled())) {
			predicate.and(qWorkFlowStep.enabled.eq(workFlowStepDTO.getEnabled()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getProductId())
				&& workFlowStepDTO.getProductId() != 0) {
			predicate.and(qWorkFlowStep.productId.eq(workFlowStepDTO.getProductId()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getProcess())) {
			predicate.and(qWorkFlowStep.process.eq(workFlowStepDTO.getProcess()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getIdWorkFlowStep())) {
			predicate.and(qWorkFlowStep.idWorkFlowStep.eq(workFlowStepDTO.getIdWorkFlowStep()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(workFlowStepDTO.getMinAmount())) {
			predicate.and(qWorkFlowStep.minAmount.loe(workFlowStepDTO.getMinAmount()));
			predicate.and(qWorkFlowStep.maxAmount.goe(workFlowStepDTO.getMinAmount()));
		}

		Iterable<WorkFlowStep> workFlowSteps = settingWorkFlowRepository.findAll(predicate);

		List<WorkFlowStepDTO> workFlowStepDTOs = new ArrayList<>();

		workFlowSteps.forEach(newWorkFlowStep -> workFlowStepDTOs
				.add(mapper.map(newWorkFlowStep, WorkFlowStepDTO.class)));
		WorkflowStepUdfGroupeDTO newworkflowStepUdfGroupeDTO;
		List<WorkflowStepUdfGroupeDTO> WorkflowStepUdfGroupe = new ArrayList<>();

		for (WorkFlowStepDTO item : workFlowStepDTOs) {
			// find Udf
			newworkflowStepUdfGroupeDTO = new WorkflowStepUdfGroupeDTO();
			newworkflowStepUdfGroupeDTO.setIdWorkflowStep(item.getIdWorkFlowStep());

			WorkflowStepUdfGroupe = workflowStepUdfGroupeService.find(newworkflowStepUdfGroupeDTO);

			item.setWorkflowStepUdfGroupe(WorkflowStepUdfGroupe);
			// end find Udf

			// find risk type

			WorkFlowStep workFlowStep = new WorkFlowStep();
			workFlowStep.setIdWorkFlowStep(item.getIdWorkFlowStep());
			List<StepRiskSetting> lstSettingRisk =
					stepRiskSettingRepository.findByWorkFlowStep(workFlowStep);
			if (!ACMValidationUtils.isNullOrEmpty(lstSettingRisk)) {
				item.setListRiskSetting(new ArrayList<>());
				lstSettingRisk.forEach(element -> {
					RiskSettingStepDTO riskSettingStepDTO = new RiskSettingStepDTO();
					riskSettingStepDTO.setIdRiskSetting(element.getSettingTypeRisk().getId());
					riskSettingStepDTO.setLabelRisk(element.getSettingTypeRisk().getLabel());
					riskSettingStepDTO.setIsEditable(element.getEditable());

					item.getListRiskSetting().add(riskSettingStepDTO);

				});

			}

		}

		return workFlowStepDTOs;

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingWorkFlowService#saveCollectionSteps(java.util.List)
	 */
	@Override
	public List<CollectionStepDTO> saveCollectionSteps(List<CollectionStepDTO> collectionStepDTOs,
			Long productId, String process) throws CollectionSettingException {

		logger.info("Start Save Collection Steps Methods");

		Long settingVersion = 0L;
		// check Product ID
		if (!ACMValidationUtils.isNullOrEmpty(productId)
				&& !ACMValidationUtils.isNullOrEmpty(process)) {

			// disable old setting
			BooleanBuilder predicate = new BooleanBuilder();
			QCollectionStep qCollectionStep = QCollectionStep.collectionStep;
			predicate.and(qCollectionStep.enabled.eq(Boolean.TRUE));
			predicate.and(qCollectionStep.productId.eq(productId));
			predicate.and(qCollectionStep.process.eq(process));
			Iterable<CollectionStep> collectionSteps =
					settingCollectionRepository.findAll(predicate);

			if (!ACMValidationUtils.isNullOrEmpty(collectionSteps)) {
				settingVersion = collectionSteps.iterator().next().getProcessVersion();
				collectionSteps.forEach(step -> {
					step.setEnabled(Boolean.FALSE);
				});
				settingCollectionRepository.saveAll(collectionSteps);
			}

		}
		else {
			logger.error("Error save Steps with Product ID NULL ");
			throw new CollectionSettingException(
					new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
							CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION,
							new TechnicalException()),
					CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION);
		}

		// check step list empty of null
		if (ACMValidationUtils.isNullOrEmpty(collectionStepDTOs)) {
			return new ArrayList<>();
		}

		UserDTO userDTO = userClient.find();
		// prepare list of steps for save
		List<CollectionStep> collectionSteps = new ArrayList<>();
		final Long newVersion = settingVersion + 1;
		collectionStepDTOs.forEach(collectionStepDTO -> {
			collectionStepDTO.setProcessVersion(newVersion);
			collectionStepDTO.setIdCollectionStep(null);
			collectionSteps.add((CollectionStep) CommonFunctions.mapperToSave(
					(mapper.map(collectionStepDTO, CollectionStep.class)), userClient, logger,
					userDTO));
		});

		// save new settings
		List<CollectionStep> newcollectionSteps =
				settingCollectionRepository.saveAll(collectionSteps);
		List<CollectionStepDTO> newollectionStepDTOs = new ArrayList<>();

		newcollectionSteps.forEach(newWorkFlowStep -> newollectionStepDTOs
				.add(mapper.map(newWorkFlowStep, CollectionStepDTO.class)));

		List<WorkflowStepUdfGroupeDTO> newWorkFlowCollectionStepDTO = new ArrayList<>();
		Collections.sort(newollectionStepDTOs, Comparator.comparing(CollectionStepDTO::getOrder));
		Collections.sort(newcollectionSteps, Comparator.comparing(CollectionStep::getOrder));
		for (int i = 0; i < collectionStepDTOs.size(); i++) {
			for (WorkflowStepUdfGroupeDTO item : collectionStepDTOs.get(i)
					.getWorkflowStepUdfGroupe()) {
				item.setIdCollectionStep(newcollectionSteps.get(i).getIdCollectionStep());
				newWorkFlowCollectionStepDTO.add(item);
			}

		}
		workflowStepUdfGroupeService.saveAll(newWorkFlowCollectionStepDTO);

		logger.info("End Save Collection Steps Methods");

		return newollectionStepDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingWorkFlowService#findCollectionStepsByProduct(java.lang.Long)
	 */
	@Override
	public List<CollectionStepDTO> findCollectionStepsByProduct(CollectionStepDTO collectionStepDTO)
			throws CollectionSettingException {

		// check step list empty of null
		// if (ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getProductId())) {
		// logger.error("Error find steps with productId equal NULL");
		// throw new CollectionSettingException(
		// new ExceptionResponseMessage(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION,
		// CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION,
		// new TechnicalException()),
		// CommonExceptionsMessage.WORKFLOW_SETTING_EXCEPTION);
		// }

		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();
		QCollectionStep qCollectionStep = QCollectionStep.collectionStep;

		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getIdCollectionStep())) {
			predicate.and(
					qCollectionStep.idCollectionStep.eq(collectionStepDTO.getIdCollectionStep()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getEnabled())) {
			predicate.and(qCollectionStep.enabled.eq(collectionStepDTO.getEnabled()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getProductId())) {
			predicate.and(qCollectionStep.productId.eq(collectionStepDTO.getProductId()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getProcess())) {
			predicate.and(qCollectionStep.process.eq(collectionStepDTO.getProcess()));
		}

		Iterable<CollectionStep> collectionSteps = settingCollectionRepository.findAll(predicate);

		List<CollectionStepDTO> collectionStepDTOs = new ArrayList<>();

		collectionSteps.forEach(newCollectionStep -> collectionStepDTOs
				.add(mapper.map(newCollectionStep, CollectionStepDTO.class)));

		for (CollectionStepDTO collectionStepDTO2 : collectionStepDTOs) {
			WorkflowStepUdfGroupeDTO newworkflowStepUdfGroupeDTO = new WorkflowStepUdfGroupeDTO();
			newworkflowStepUdfGroupeDTO
					.setIdCollectionStep(collectionStepDTO2.getIdCollectionStep());
			List<WorkflowStepUdfGroupeDTO> WorkflowStepUdfGroupe = new ArrayList<>();

			WorkflowStepUdfGroupe = workflowStepUdfGroupeService.find(newworkflowStepUdfGroupeDTO);

			collectionStepDTO2.setWorkflowStepUdfGroupe(WorkflowStepUdfGroupe);

		}

		return collectionStepDTOs;
	}

	/**
	 * Builds the query.
	 *
	 * @param collectionStepDTO the collection step DTO
	 * @param qCollectionStep the q collection step
	 * @return the boolean builder
	 */
	private BooleanBuilder buildQuery(CollectionStepDTO collectionStepDTO,
			QCollectionStep qCollectionStep) {

		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find by enabled data
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getEnabled())) {
			predicate.and(qCollectionStep.enabled.eq(collectionStepDTO.getEnabled()));
		}

		// find by process type
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getProcess())) {
			predicate.and(qCollectionStep.process.eq(collectionStepDTO.getProcess()));
		}
		// find by collectionStepID
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getIdCollectionStep())) {
			predicate.and(
					qCollectionStep.idCollectionStep.eq(collectionStepDTO.getIdCollectionStep()));
		}
		// find by ProductID
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getProductId())) {
			predicate.and(qCollectionStep.productId.eq(collectionStepDTO.getProductId()));
		}
		// find by StepOrder
		if (!ACMValidationUtils.isNullOrEmpty(collectionStepDTO.getOrder())) {
			predicate.and(qCollectionStep.order.eq(collectionStepDTO.getOrder()));
		}
		return predicate;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingWorkFlowService#findSettingCollection(com.acm.utils.dtos.
	 * CollectionStepDTO)
	 */
	@Override
	public List<CollectionStepDTO> findSettingCollection(CollectionStepDTO collectionStepDTO) {

		// build Predicate using given params
		BooleanBuilder predicate = new BooleanBuilder();

		QCollectionStep qCollectionStep = QCollectionStep.collectionStep;
		predicate = buildQuery(collectionStepDTO, qCollectionStep);
		Iterable<CollectionStep> collectionSteps = settingCollectionRepository.findAll(predicate);

		List<CollectionStepDTO> collectionStepDTOs = new ArrayList<>();

		collectionSteps.forEach(newCollectionStep -> collectionStepDTOs
				.add(mapper.map(newCollectionStep, CollectionStepDTO.class)));

		return collectionStepDTOs;
	}

}
