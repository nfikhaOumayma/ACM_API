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
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AddressRepository;
import com.acm.service.AddressHistoriqueService;
import com.acm.service.AddressService;
import com.acm.service.CustomerService;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressHistoriqueDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.models.Address;
import com.acm.utils.models.QAddress;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

// TODO: Auto-generated Javadoc
/**
 * {@link AddressServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class AddressServiceImpl implements AddressService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(AddressServiceImpl.class);

	/** The address repository. */
	@Autowired
	private AddressRepository addressRepository;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The address historique service. */
	@Autowired
	private AddressHistoriqueService addressHistoriqueService;

	/**
	 * Find.
	 *
	 * @param id the id
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#find(java.lang.Long)
	 */
	@Override
	public AddressDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find Address by ID : {}", id);
		Address address = addressRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(address)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Address.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ Address.class.getSimpleName() + CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(address, AddressDTO.class);
	}

	/**
	 * Find.
	 *
	 * @param addressDTO the address DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#find(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public List<AddressDTO> find(AddressDTO addressDTO) {

		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QAddress
		QAddress qAddress = QAddress.address;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qAddress.enabled.eq(Boolean.TRUE));

		// find by CustomerId
		if (!ACMValidationUtils.isNullOrEmpty(addressDTO.getCustomerId())) {
			predicate.and(qAddress.customerId.eq(addressDTO.getCustomerId()));
		}

		// find Primary
		if (!ACMValidationUtils.isNullOrEmpty(addressDTO.getIsPrimary())) {
			predicate.and(qAddress.isPrimary.eq(addressDTO.getIsPrimary()));
		}

		// find by Id Address Abacus
		if (!ACMValidationUtils.isNullOrEmpty(addressDTO.getIdAddressAbacus())) {
			predicate.and(qAddress.idAddressAbacus.eq(addressDTO.getIdAddressAbacus()));
		}

		Iterable<Address> iterable = addressRepository.findAll(predicate);
		List<Address> addresss = new ArrayList<>();
		iterable.forEach(addresss::add);
		logger.info("{} : Address was founded", addresss.size());

		List<AddressDTO> addresssDTOs = new ArrayList<>();
		addresss.forEach(address -> addresssDTOs.add(mapper.map(address, AddressDTO.class)));
		return addresssDTOs;
	}

	/**
	 * Save.
	 *
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#save(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public AddressDTO save(AddressDTO addressDTO) {

		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		Address address = mapper.map(addressDTO, Address.class);

		CommonFunctions.mapperToSave(address, userClient, logger);
		Address newAddress = addressRepository.save(address);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Address.class.getSimpleName());
		return mapper.map(newAddress, AddressDTO.class);
	}

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#save(java.lang.Long, com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public AddressDTO save(Long id, AddressDTO addressDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update Address with ID = {}", id);
		Address oldAddress = addressRepository.findById(id).orElse(null);

		// check if object is null
		if (oldAddress == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Address.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Address.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldAddress)
		mapper.map(addressDTO, oldAddress);
		CommonFunctions.mapperToUpdate(oldAddress, userClient, logger);

		// update & persist data in DB
		Address newAddress = addressRepository.save(oldAddress);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Address.class.getSimpleName());
		return mapper.map(newAddress, AddressDTO.class);
	}

	/**
	 * Save by batch.
	 *
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#saveByBatch(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public AddressDTO saveByBatch(AddressDTO addressDTO) {

		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// find customer by CustomerId ABACUS
		List<CustomerDTO> customerDTOs =
				customerService.findCustomerIdExtern(addressDTO.getCustomerId(), null);
		if (!ACMValidationUtils.isNullOrEmpty(customerDTOs)) {
			// setting customer id ACM
			addressDTO.setCustomerId(customerDTOs.get(0).getId());

			// check if exist address
			List<AddressDTO> addressDTOs = find(addressDTO);
			if (!ACMValidationUtils.isNullOrEmpty(addressDTOs)) {
				return addressDTOs.get(0);
			}

			// insert data
			AddressDTO newAddressDTO = save(addressDTO);
			logger.info("{} has been successfully created", newAddressDTO);

			return newAddressDTO;
		}
		logger.warn(
				"### Failed while saving ADDRESS ### Failed to find CUSTOMER by the given customer Id Extern = {}",
				addressDTO.getCustomerId());
		return addressDTO;
	}

	/**
	 * Delete all.
	 *
	 * @param customerId the customer id
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#deleteAll(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public void deleteAll(Long customerId) {

		Preconditions.checkNotNull(customerId, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		addressRepository.deleteByCustomerId(customerId);
		logger.info("Delete All addressDTOs  with ID = {} and ID_ADDRESS_ABACUS is NULL",
				customerId);
	}

	/**
	 * Delete.
	 *
	 * @param addressDTO the address DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#delete(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public void delete(AddressDTO addressDTO) {

		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		addressRepository.delete(new Address(addressDTO.getId()));
		logger.info("Delete address with ID = {}", addressDTO.getId());

	}

	/**
	 * Save all.
	 *
	 * @param addressDTO the address DTO
	 * @return the address DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.AddressService#saveAll(com.acm.utils.dtos.AddressDTO)
	 */
	@Override
	public AddressDTO saveAll(AddressDTO addressDTO) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(addressDTO, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		// Find old Address
		logger.info("Find Address with ID = {}", addressDTO.getId());
		Address oldAddress = addressRepository.findById(addressDTO.getId()).orElse(null);
		// check if object is null
		if (oldAddress == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT, Address.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + Address.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + addressDTO.getId());
		}
		AddressDTO oldAddressDTO = mapper.map(oldAddress, AddressDTO.class);
		// Check address data null
		if (ACMValidationUtils.isNullOrEmpty(addressDTO.getLan())) {
			addressDTO.setLan("");
		}
		if (ACMValidationUtils.isNullOrEmpty(addressDTO.getLng())) {
			addressDTO.setLng("");
		}
		if (ACMValidationUtils.isNullOrEmpty(oldAddressDTO.getLan())) {
			oldAddressDTO.setLan("");
		}
		if (ACMValidationUtils.isNullOrEmpty(oldAddressDTO.getLng())) {
			oldAddressDTO.setLng("");
		}
		// CHECK LATITUDE LONGITUDE changement
		if (!addressDTO.getLan().equals(oldAddressDTO.getLan())
				|| !addressDTO.getLng().equals(oldAddressDTO.getLng())) {
			// save address historique
			addressHistoriqueService.save(new AddressHistoriqueDTO(addressDTO.getId(),
					oldAddressDTO, addressDTO, addressDTO.getReasonUpdate()));
			// update address
			// mapping new data with existing data (oldAddress)
			mapper.map(addressDTO, oldAddress);
			CommonFunctions.mapperToUpdate(oldAddress, userClient, logger);

			// update & persist data in DB
			Address updatedAddress = addressRepository.save(oldAddress);

			logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, Address.class.getSimpleName());
			return mapper.map(updatedAddress, AddressDTO.class);
		}
		return addressDTO;
	}

	/**
	 * Map lst DTO to lst entity.
	 *
	 * @param lstDto the lst dto
	 * @return the list
	 */
	public List<Address> mapLstDTOToLstEntity(List<AddressDTO> lstDto) {

		return lstDto.stream().map((item) -> mapper.map(item, Address.class))
				.collect(Collectors.toList());
	}

	/**
	 * Map lst entity to lst DTO.
	 *
	 * @param lstEntity the lst entity
	 * @return the list
	 */
	public List<AddressDTO> mapLstEntityToLstDTO(List<Address> lstEntity) {

		return lstEntity.stream().map((item) -> mapper.map(item, AddressDTO.class))
				.collect(Collectors.toList());
	}

	/**
	 * Save all address.
	 *
	 * @param addressDTOlst the address DT olst
	 * @return the list
	 */
	@Override
	public List<AddressDTO> saveAllAddress(List<AddressDTO> addressDTOlst) {

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Address.class.getSimpleName());
		List<Address> l = mapLstDTOToLstEntity(addressDTOlst);
		l.forEach((item) -> {
			item.setEnabled(true);
			item.setCustomerId(new Long(0));
			item.setIsPrimary(false);
		});
		return mapLstEntityToLstDTO(addressRepository.saveAll(l));
	}

}
