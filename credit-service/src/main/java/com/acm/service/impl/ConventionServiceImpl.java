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

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.ConventionRepository;
import com.acm.service.AcmDocumentsService;
import com.acm.service.ConventionService;
import com.acm.utils.dtos.ConventionDTO;
import com.acm.utils.models.Convention;

/**
 * {@link ConventionServiceImpl} ConventionServiceImpl.
 *
 * @author KhaledOuali
 * @since 1.12
 */
@Service
public class ConventionServiceImpl implements ConventionService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(ConventionServiceImpl.class);

	/** The customer repository. */
	@Autowired
	private ConventionRepository conventionRepository;

	/** The acm documents service. */
	@Autowired
	private AcmDocumentsService acmDocumentsService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/**
	 * Map lst DTO to lst entity.
	 *
	 * @param lstDto the lst dto
	 * @return the list
	 */

	public List<Convention> mapLstDTOToLstEntity(List<ConventionDTO> lstDto) {

		return lstDto.stream().map((item) -> mapper.map(item, Convention.class))
				.collect(Collectors.toList());
	}

	/**
	 * Map lst entity to lst DTO.
	 *
	 * @param lstEntity the lst entity
	 * @return the list
	 */
	public List<ConventionDTO> mapLstEntityToLstDTO(List<Convention> lstEntity) {

		return lstEntity.stream().map((item) -> mapper.map(item, ConventionDTO.class))
				.collect(Collectors.toList());
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConventionService#saveAll(java.util.List)
	 */
	@Override
	public List<ConventionDTO> saveAll(List<ConventionDTO> lstConventionDTO) {

		List<ConventionDTO> lstConventionToReturn = new ArrayList<>();
		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Convention.class.getSimpleName());
		lstConventionDTO.forEach(elem -> {
			Convention convention = conventionRepository.save(mapper.map(elem, Convention.class));
			ConventionDTO conventiondto = mapper.map(convention, ConventionDTO.class);
			conventiondto.setListDocsType(elem.getListDocsType());
			lstConventionToReturn.add(conventiondto);

		});

		return lstConventionToReturn;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConventionService#find(java.lang.Long)
	 */
	@Override
	public ConventionDTO find(Long id) throws ResourcesNotFoundException {

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConventionService#find(com.acm.utils.dtos.ConventionDTO)
	 */
	@Override
	public List<ConventionDTO> find(ConventionDTO conventionDTO) {

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConventionService#save(java.lang.Long, com.acm.utils.dtos.ConventionDTO)
	 */
	@Override
	public ConventionDTO save(Long id, ConventionDTO conventionDTO) {

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.ConventionService#findByIdSupplier(java.lang.Long)
	 */
	@Override
	public List<ConventionDTO> findByIdSupplier(Long idSupplier) {

		return mapLstEntityToLstDTO(conventionRepository.findByIdSupplier(idSupplier));
	}

}
