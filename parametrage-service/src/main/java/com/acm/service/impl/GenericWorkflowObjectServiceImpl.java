/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.acm.client.UserClient;
import com.acm.constants.common.CommonFunctions;
import com.acm.repository.GenericWorkflowObjectRepository;
import com.acm.service.GenericWorkflowObjectService;
import com.acm.utils.dtos.GenericWorkFlowObjectDTO;
import com.acm.utils.models.GenericWorkFlowObject;

/**
 * {@link genericWorkflowObjectServiceImpl} Class Impl.
 *
 * @author ManelLamloum
 * @since 1.0.14
 */
@Service
public class GenericWorkflowObjectServiceImpl implements GenericWorkflowObjectService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(GenericWorkflowObjectServiceImpl.class);

	/** The user client. */
	@Autowired
	private UserClient userClient;
	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The acm ihm field repository. */
	@Autowired
	private GenericWorkflowObjectRepository genericWorkflowObjectRepository;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GenericWorkflowObjectService#save(com.acm.utils.dtos.
	 * GenericWorkFlowObjectDTO)
	 */
	@Override
	public GenericWorkFlowObjectDTO save(GenericWorkFlowObjectDTO genericWorkFlowObjectDTO) {

		GenericWorkFlowObject genericWorkFlowObject =
				mapper.map(genericWorkFlowObjectDTO, GenericWorkFlowObject.class);
		genericWorkFlowObject.setEnabled(true);
		// genericWorkFlowObject =(GenericWorkFlowObject)
		// CommonFunctions.mapperToSave(genericWorkFlowObject,userClient,logger) ;
		return mapper.map(
				genericWorkflowObjectRepository.save((GenericWorkFlowObject) CommonFunctions
						.mapperToSave(genericWorkFlowObject, userClient, logger)),
				GenericWorkFlowObjectDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.GenericWorkflowObjectService#findAll()
	 */
	@Override
	public List<GenericWorkFlowObjectDTO> findAll() {

		List<GenericWorkFlowObject> GenericWorkflowObjects =
				genericWorkflowObjectRepository.findAll();
		List<GenericWorkFlowObjectDTO> GenericWorkflowObjectDTOs = new ArrayList<>();
		for (GenericWorkFlowObject item : GenericWorkflowObjects) {
			GenericWorkflowObjectDTOs.add(mapper.map(item, GenericWorkFlowObjectDTO.class));
		}
		return GenericWorkflowObjectDTOs;
	}

}
