/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.WorkflowStepUdfGroupeRepository;
import com.acm.service.UserDefinedFieldGroupService;
import com.acm.service.UserDefinedFieldListValuesService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.service.WorkflowStepUdfGroupeService;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.WorkflowStepUdfGroupeDTO;
import com.acm.utils.enums.SettingUDFFieldsType;
import com.acm.utils.models.QWorkflowStepUdfGroupe;
import com.acm.utils.models.WorkflowStepUdfGroupe;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * The Class WorkflowStepUdfGroupeServiceImpl.
 */
@Service
public class WorkflowStepUdfGroupeServiceImpl implements WorkflowStepUdfGroupeService {
	/** The Constant logger. */

	private static final Logger logger =
			LoggerFactory.getLogger(WorkflowStepUdfGroupeServiceImpl.class);

	/** The user defined field group service. */
	@Autowired
	UserDefinedFieldGroupService userDefinedFieldGroupService;

	/** The user defined fields service. */
	@Autowired
	UserDefinedFieldsService userDefinedFieldsService;
	/** The workflow step udf grouperepository. */
	@Autowired
	private WorkflowStepUdfGroupeRepository workflowStepUdfGrouperepository;
	/** The user defined field list values service. */
	@Autowired
	private UserDefinedFieldListValuesService userDefinedFieldListValuesService;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/**
	 * Find.
	 *
	 * @param workflowStepUdfGroupeDTO the workflow step udf groupe DTO
	 * @return the workflow step udf groupe DTO
	 */

	@Override
	public WorkflowStepUdfGroupeDTO save(WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO) {

		Preconditions.checkNotNull(workflowStepUdfGroupeDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		WorkflowStepUdfGroupe workflowStepUdfGroupe =
				mapper.map(workflowStepUdfGroupeDTO, WorkflowStepUdfGroupe.class);

		WorkflowStepUdfGroupe newWorkflowStepUdfGroupe =
				workflowStepUdfGrouperepository.save(workflowStepUdfGroupe);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				WorkflowStepUdfGroupe.class.getSimpleName());
		return mapper.map(newWorkflowStepUdfGroupe, WorkflowStepUdfGroupeDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.WorkflowStepUdfGroupeService#saveAll(java.util.List)
	 */
	@Override
	public List<WorkflowStepUdfGroupeDTO> saveAll(
			List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupeDTOs) {

		List<WorkflowStepUdfGroupe> workflowStepUdfGroupesList = new ArrayList<>();
		List<WorkflowStepUdfGroupeDTO> newWorkflowStepUdfGroupeDTOs = new ArrayList<>();
		for (WorkflowStepUdfGroupeDTO item : workflowStepUdfGroupeDTOs) {
			Preconditions.checkNotNull(item);
			WorkflowStepUdfGroupe workflowStepUdfGroupes =
					mapper.map(item, WorkflowStepUdfGroupe.class);

			workflowStepUdfGroupesList.add(workflowStepUdfGroupes);

			newWorkflowStepUdfGroupeDTOs
					.add(mapper.map(workflowStepUdfGroupes, WorkflowStepUdfGroupeDTO.class));
		}
		workflowStepUdfGrouperepository.saveAll(workflowStepUdfGroupesList);
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE,
				WorkflowStepUdfGroupe.class.getSimpleName());
		return workflowStepUdfGroupeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.WorkflowStepUdfGroupeService#find(com.acm.utils.dtos.
	 * WorkflowStepUdfGroupeDTO)
	 */
	@Override
	public List<WorkflowStepUdfGroupeDTO> find(WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO) {

		// init QSettingDocumentProduct
		QWorkflowStepUdfGroupe qWorkflowStepUdfGroupe =
				QWorkflowStepUdfGroupe.workflowStepUdfGroupe;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find by ID
		if (!ACMValidationUtils
				.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdWorkFlowStepUdfGroup())) {
			predicate.and(qWorkflowStepUdfGroupe.idWorkFlowStepUdfGroup
					.eq(workflowStepUdfGroupeDTO.getIdWorkFlowStepUdfGroup()));
		}

		// find by id produit
		if (!ACMValidationUtils
				.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdUserDefinedFieldGroup())) {
			predicate.and(qWorkflowStepUdfGroupe.idUserDefinedFieldGroup
					.eq(workflowStepUdfGroupeDTO.getIdUserDefinedFieldGroup()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdUserDefinedFields())) {
			predicate.and(qWorkflowStepUdfGroupe.idUserDefinedFields
					.eq(workflowStepUdfGroupeDTO.getIdUserDefinedFields()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdWorkflowStep())) {
			predicate.and(qWorkflowStepUdfGroupe.idWorkflowStep
					.eq(workflowStepUdfGroupeDTO.getIdWorkflowStep()));
		}
		if (!ACMValidationUtils.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdCollectionStep())) {
			predicate.and(qWorkflowStepUdfGroupe.idCollectionStep
					.eq(workflowStepUdfGroupeDTO.getIdCollectionStep()));
		}

		// QueryDSL using springDATA
		Iterable<WorkflowStepUdfGroupe> iterable =
				workflowStepUdfGrouperepository.findAll(predicate);
		List<WorkflowStepUdfGroupe> workflowStepUdfGroupes = new ArrayList<>();
		iterable.forEach(workflowStepUdfGroupes::add);
		logger.info("{} : Workflow Step Udf Groupe was founded", workflowStepUdfGroupes.size());

		// mapping returned list
		List<WorkflowStepUdfGroupeDTO> workflowStepUdfGroupeDTOs = new ArrayList<>();
		workflowStepUdfGroupes.forEach(workflowStepUdfGroupe -> workflowStepUdfGroupeDTOs
				.add(mapper.map(workflowStepUdfGroupe, WorkflowStepUdfGroupeDTO.class)));

		logger.info("Returning founded data ...");
		return workflowStepUdfGroupeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.WorkflowStepUdfGroupeService#findUdfGroupsByStepId(com.acm.utils.dtos.
	 * WorkflowStepUdfGroupeDTO)
	 */
	@Override
	public List<UserDefinedFieldGroupDTO> findUdfGroupsByStepId(
			WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO) {

		List<WorkflowStepUdfGroupe> workflowStepUdfGroupes;
		// case of 'LOAN || TOPUP' workflow
		if (!ACMValidationUtils.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdWorkflowStep())) {
			// find udfGroups'ids of a specific step (By IdWorkflowStep)
			workflowStepUdfGroupes = workflowStepUdfGrouperepository
					.findByIdWorkflowStepAndIdUserDefinedFieldsIsNull(
							workflowStepUdfGroupeDTO.getIdWorkflowStep());
		}
		// case of 'COLLECTION || LEGAL' workflow
		else if (!ACMValidationUtils
				.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdCollectionStep())) {
			// find udfGroups'ids of a specific step (By IdCollectionStep)
			workflowStepUdfGroupes = workflowStepUdfGrouperepository
					.findByIdCollectionStepAndIdUserDefinedFieldsIsNull(
							workflowStepUdfGroupeDTO.getIdCollectionStep());
		}
		else {
			return new ArrayList<>();
		}

		List<Long> udfGroupIds = workflowStepUdfGroupes.stream()
				.map(WorkflowStepUdfGroupe::getIdUserDefinedFieldGroup)
				.collect(Collectors.toList());
		// find udfGroups of the specified step
		List<UserDefinedFieldGroupDTO> udfGroupDTOs =
				userDefinedFieldGroupService.findByIds(udfGroupIds);
		// set mandatory attribute of the UdfGroup according to mandatory attribute set in setting
		// Workflow UDF
		for (UserDefinedFieldGroupDTO udfG : udfGroupDTOs) {
			WorkflowStepUdfGroupe element = workflowStepUdfGroupes.stream()
					.filter(udfSetting -> !ACMValidationUtils
							.isNullOrEmpty(udfSetting.getIdUserDefinedFieldGroup())
							&& udfSetting.getIdUserDefinedFieldGroup().equals(udfG.getId()))
					.findFirst().orElse(null);
			if (!ACMValidationUtils.isNullOrEmpty(element)) {
				// set mandatory field
				udfG.setMondatory(element.getMandatory());
			}
		}
		return udfGroupDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.WorkflowStepUdfGroupeService#findUdfFieldsByStepId(com.acm.utils.dtos.
	 * WorkflowStepUdfGroupeDTO, java.lang.Long)
	 */
	@Override
	public List<UserDefinedFieldsDTO> findUdfFieldsByStepId(
			WorkflowStepUdfGroupeDTO workflowStepUdfGroupeDTO, Long udfGroupId) {

		List<WorkflowStepUdfGroupe> workflowStepUdfGroupes;
		// case of 'LOAN || TOPUP' workflow
		if (!ACMValidationUtils.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdWorkflowStep())) {
			workflowStepUdfGroupes = workflowStepUdfGrouperepository
					.findByIdWorkflowStepAndIdUserDefinedFieldGroupAndIdUserDefinedFieldsIsNotNull(
							workflowStepUdfGroupeDTO.getIdWorkflowStep(), udfGroupId);
		}
		// case of 'COLLECTION || LEGAL' workflow
		else if (!ACMValidationUtils
				.isNullOrEmpty(workflowStepUdfGroupeDTO.getIdCollectionStep())) {
			workflowStepUdfGroupes = workflowStepUdfGrouperepository
					.findByIdCollectionStepAndIdUserDefinedFieldGroup(
							workflowStepUdfGroupeDTO.getIdCollectionStep(), udfGroupId);
		}
		else {
			return new ArrayList<>();
		}
		List<Long> udfFieldIds = workflowStepUdfGroupes.stream()
				.map(WorkflowStepUdfGroupe::getIdUserDefinedFields).collect(Collectors.toList());
		// find udfFields of the specified step and of the udfGroup in parameter (udfGroupId
		// parameter)
		List<UserDefinedFieldsDTO> udfFieldsDTOs =
				userDefinedFieldsService.findUDFFieldByIds(udfFieldIds);
		// set mandatory attribute of the Udf fields according to mandatory attribute set in setting
		// Workflow UDF
		for (UserDefinedFieldsDTO udfF : udfFieldsDTOs) {
			WorkflowStepUdfGroupe element = workflowStepUdfGroupes.stream()
					.filter(udfSetting -> !ACMValidationUtils
							.isNullOrEmpty(udfSetting.getIdUserDefinedFields())
							&& udfSetting.getIdUserDefinedFields().equals(udfF.getId()))
					.findFirst().orElse(null);
			if (!ACMValidationUtils.isNullOrEmpty(element)) {
				// set mandatory field
				udfF.setMandatory(element.getMandatory());
			}

			// loading udf list value if exist
			if (udfF.getFieldType().equals(SettingUDFFieldsType.LIST.typeId())
					&& udfF.getIdUDFListValue() != 0) {
				udfF.getFieldListValuesDTOs().addAll(userDefinedFieldListValuesService
						.find(new UserDefinedFieldListValuesDTO(udfF.getIdUDFListValue(), null)));
			}
		}

		return udfFieldsDTOs;
	}
}
