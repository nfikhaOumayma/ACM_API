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
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.repository.AddressHistoriqueRepository;
import com.acm.service.AddressHistoriqueService;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressHistoriqueDTO;
import com.acm.utils.models.AddressHistorique;
import com.acm.utils.models.QAddressHistorique;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AddressHistoriqueServiceImpl} Class Impl.
 *
 * @author YesserSomai
 * @since 1.0.14
 */
@Service
public class AddressHistoriqueServiceImpl implements AddressHistoriqueService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(AddressHistoriqueServiceImpl.class);

	/** The addressHistorique repository. */
	@Autowired
	private AddressHistoriqueRepository addressHistoriqueRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressHistoriqueService#find(com.acm.utils.dtos.AddressHistoriqueDTO)
	 */
	@Override
	public List<AddressHistoriqueDTO> find(AddressHistoriqueDTO addressHistoriqueDTO) {

		Preconditions.checkNotNull(addressHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		logger.info("start find address Historique");
		// find by Address ACM Id
		if (ACMValidationUtils.isNullOrEmpty(addressHistoriqueDTO.getIdAddressACM())) {
			return new ArrayList<>();
		}

		// init QAddressHistorique
		QAddressHistorique qAddressHistorique = QAddressHistorique.addressHistorique;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAddressHistorique.idAddressACM.eq(addressHistoriqueDTO.getIdAddressACM()));

		// mapping founded data
		Iterable<AddressHistorique> iterable = addressHistoriqueRepository.findAll(predicate);
		List<AddressHistorique> addressHistoriques = new ArrayList<>();
		iterable.forEach(addressHistoriques::add);
		logger.info("{} : AddressHistorique was founded", addressHistoriques.size());

		// create return object (convert string to JSON old address and new address)
		List<AddressHistoriqueDTO> addressHistoriquesDTOs = new ArrayList<>();
		addressHistoriques
				.forEach(addressHistorique -> addressHistoriquesDTOs.add(new AddressHistoriqueDTO(
						addressHistorique.getId(), addressHistorique.getIdAddressACM(),
						(AddressDTO) CommonFunctions.convertJSONStringtoObject(
								addressHistorique.getOldAddress(), AddressDTO.class),
						(AddressDTO) CommonFunctions.convertJSONStringtoObject(
								addressHistorique.getNewAddress(), AddressDTO.class),
						addressHistorique.getDateInsertion(), addressHistorique.getInsertBy(),
						addressHistorique.getReasonUpdate())));
		logger.info("{} : AddressHistoriqueDTO was founded", addressHistoriquesDTOs.size());
		logger.info("find address Historique :: done");

		// retruning data
		return addressHistoriquesDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressHistoriqueService#save(com.acm.utils.dtos.AddressHistoriqueDTO)
	 */
	@Override
	public AddressHistoriqueDTO save(AddressHistoriqueDTO addressHistoriqueDTO) {

		logger.info("start save address Historique");
		Preconditions.checkNotNull(addressHistoriqueDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// mapping data
		AddressHistorique addressHistorique =
				mapper.map(addressHistoriqueDTO, AddressHistorique.class);
		// object to save
		CommonFunctions.mapperToSave(addressHistorique, userClient, logger);
		// convert old address to JSON
		addressHistorique.setOldAddress(
				CommonFunctions.convertObjectToJSONString(addressHistoriqueDTO.getOldAddressDTO()));
		// convert new address to JSON
		addressHistorique.setNewAddress(
				CommonFunctions.convertObjectToJSONString(addressHistoriqueDTO.getNewAddressDTO()));
		// save data
		AddressHistorique newAddressHistorique =
				addressHistoriqueRepository.save(addressHistorique);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, AddressHistorique.class.getSimpleName());
		return mapper.map(newAddressHistorique, AddressHistoriqueDTO.class);
	}
}
