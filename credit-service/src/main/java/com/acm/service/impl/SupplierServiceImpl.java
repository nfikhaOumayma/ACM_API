/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.persistence.EntityManager;
import javax.persistence.Query;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SupplierRepository;
import com.acm.service.AddressService;
import com.acm.service.CustomerService;
import com.acm.service.SupplierService;
import com.acm.service.UserDefinedFieldsLinksService;
import com.acm.utils.date.DateUtil;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.SupplierDTO;
import com.acm.utils.dtos.pagination.SupplierPaginationDTO;
import com.acm.utils.models.QSupplier;
import com.acm.utils.models.Supplier;
import com.acm.utils.validation.ACMValidationUtils;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.StringTemplate;

/**
 * The Class SupplierServiceImpl.
 */
@Service
public class SupplierServiceImpl implements SupplierService {

	/** The Constant logger. */
	private static final Logger logger = LoggerFactory.getLogger(SupplierServiceImpl.class);

	/** The customer repository. */
	@Autowired
	private SupplierRepository supplierRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The address service. */
	@Autowired
	private AddressService addressService;
	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The parametrage client. */
	@Autowired
	private ParametrageClient parametrageClient;

	/** The user defined fields links service. */
	@Autowired
	private UserDefinedFieldsLinksService userDefinedFieldsLinksService;

	/** The customer service. */
	@Autowired
	private CustomerService customerService;

	/**
	 * Save.
	 *
	 * @param supplierDTO the supplier DTO
	 * @return the supplier DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws CreditException the credit exception
	 * @throws CalculateAgeException the calculate age exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SupplierService#save(com.acm.utils.dtos.SupplierDTO)
	 */
	@Override
	public SupplierDTO save(SupplierDTO supplierDTO)
			throws ResourcesNotFoundException, CreditException, CalculateAgeException {

		CustomerDTO isCustomerDTO = checkIsCustomer(supplierDTO);
		CustomerDTO isNotCustomerDTO = checkIsNotCustomer(supplierDTO);

		if (!ACMValidationUtils.isNullOrEmpty(isCustomerDTO)) {
			supplierDTO.setIsCustomer(Boolean.TRUE);
		}
		else {
			supplierDTO.setIsCustomer(Boolean.FALSE);
		}

		SupplierDTO newSupplierDTO = new SupplierDTO();
		Supplier supplier = mapper.map(supplierDTO, Supplier.class);
		CommonFunctions.mapperToSave(supplier, userClient, logger);
		Supplier supplierNew = new Supplier();
		try {
			supplierNew = supplierRepository.save(supplier);
		}
		catch (DataIntegrityViolationException e) {
			throw new CreditException(new ExceptionResponseMessage(CommonErrorCode.SUPPLIER_EXIST,
					CommonExceptionsMessage.SUPPLIER_EXIST_ALREADY, new TechnicalException()),
					CommonExceptionsMessage.SUPPLIER_EXIST_ALREADY);
		}
		newSupplierDTO = mapper.map(supplierNew, SupplierDTO.class);
		// save Address
		List<AddressDTO> newAddressDTOs = new ArrayList<>();

		for (AddressDTO addressDTO : supplierDTO.getListAddress()) {
			addressDTO.setSupplier(newSupplierDTO);
			addressDTO.setCustomerId(0L);
			if (ACMValidationUtils.isNullOrEmpty(addressDTO.getIsPrimary())) {
				addressDTO.setIsPrimary(Boolean.TRUE);
			}
			newAddressDTOs.add(addressDTO);
		}
		addressService.saveAllAddress(newAddressDTOs);
		// save udfs
		if (!ACMValidationUtils.isNullOrEmpty(supplierDTO.getUserDefinedFieldsLinksDTOs())) {
			userDefinedFieldsLinksService.updateAcmUdfLinksByElementId(
					supplierDTO.getUserDefinedFieldsLinksDTOs(), newSupplierDTO.getId(), null);
		}

		if (!ACMValidationUtils.isNullOrEmpty(isNotCustomerDTO)) {
			customerService.save(isNotCustomerDTO.getId(), isNotCustomerDTO);
		}

		if (!ACMValidationUtils.isNullOrEmpty(isCustomerDTO)) {
			customerService.save(isCustomerDTO.getId(), isCustomerDTO);
		}

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, Supplier.class.getSimpleName());
		return newSupplierDTO;
	}

	/**
	 * Check is not customer.
	 *
	 * @param supplierDTO the supplier DTO
	 * @return the customer DTO
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public CustomerDTO checkIsNotCustomer(SupplierDTO supplierDTO)
			throws CalculateAgeException, CreditException, ResourcesNotFoundException {

		// Check supplier is Customer in case of update RNE or identity ( Remove the flag is
		// Supplier in Customer )
		if (!ACMValidationUtils.isNullOrEmpty(supplierDTO.getId())) {
			Supplier supplierIsCustomer =
					supplierRepository.findById(supplierDTO.getId()).orElse(null);
			if (!ACMValidationUtils.isNullOrEmpty(supplierIsCustomer)) {
				if (supplierIsCustomer.getIsCustomer().equals(Boolean.TRUE)) {
					if (!Objects.equals(supplierIsCustomer.getRegisterNumber(),
							supplierDTO.getRegisterNumber())
							|| !Objects.equals(supplierIsCustomer.getIdentity(),
									supplierDTO.getIdentity())) {

						CustomerDTO customerDTO = new CustomerDTO();
						if (!ACMValidationUtils
								.isNullOrEmpty(supplierIsCustomer.getRegisterNumber())) {
							customerDTO.setRegisterNumber(supplierIsCustomer.getRegisterNumber());
						}
						if (!ACMValidationUtils.isNullOrEmpty(supplierIsCustomer.getIdentity())) {
							customerDTO.setIdentity(supplierIsCustomer.getIdentity());
						}

						List<CustomerDTO> result = customerService.find(customerDTO);

						if (result.size() == 1) {
							result.get(0).setIsSupplier(Boolean.FALSE);
							return result.get(0);
							// customerService.save(result.get(0).getId(), result.get(0));
						}

					}
				}
			}
		}

		return null;
	}

	/**
	 * Check supplier.
	 *
	 * @param supplierDTO the supplier DTO
	 * @return the customer DTO
	 * @throws CalculateAgeException the calculate age exception
	 * @throws CreditException the credit exception
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	public CustomerDTO checkIsCustomer(SupplierDTO supplierDTO)
			throws CalculateAgeException, CreditException, ResourcesNotFoundException {

		// Check Supplier is Customer
		if (!ACMValidationUtils.isNullOrEmpty(supplierDTO.getIdentity())
				|| !ACMValidationUtils.isNullOrEmpty(supplierDTO.getRegisterNumber())) {

			CustomerDTO customerDTO = new CustomerDTO();
			if (!ACMValidationUtils.isNullOrEmpty(supplierDTO.getIdentity())) {
				customerDTO.setIdentity(supplierDTO.getIdentity());
			}
			if (!ACMValidationUtils.isNullOrEmpty(supplierDTO.getRegisterNumber())) {
				customerDTO.setRegisterNumber(supplierDTO.getRegisterNumber());
			}

			List<CustomerDTO> result = customerService.find(customerDTO);

			if (result.size() == 1) {
				result.get(0).setIsSupplier(Boolean.TRUE);
				return result.get(0);
			}
			else if (result.size() > 1) {
				throw new CreditException(
						new ExceptionResponseMessage(CommonErrorCode.SUPPLIER_ERROR,
								CommonExceptionsMessage.SUPPLIER_ERROR_MSG,
								new TechnicalException()),
						CommonExceptionsMessage.SUPPLIER_ERROR_MSG);
			}
		}

		return null;
	}

	/**
	 * Find.
	 *
	 * @param supplierPaginationDTO the supplier pagination DTO
	 * @return the supplier pagination DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SupplierService#find(com.acm.utils.dtos.pagination.
	 * SupplierPaginationDTO)
	 */
	@Override
	public SupplierPaginationDTO find(SupplierPaginationDTO supplierPaginationDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(supplierPaginationDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// init default PageNumber if NULL : 0 (first page)
		if (ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getPageNumber())) {
			supplierPaginationDTO.setPageNumber(0);
		}
		// init default PageSize if NULL : 10 elements
		if (ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getPageSize())) {
			supplierPaginationDTO.setPageSize(10);
		}
		// setting default data
		supplierPaginationDTO.setResultsSuppliers(new ArrayList<>());
		// setting default totals pages
		supplierPaginationDTO.setTotalElements(0L);
		// setting default totals elements
		supplierPaginationDTO.setTotalPages(0);
		// init QCustomer
		QSupplier qSupplier = QSupplier.supplier;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();
		// find only enabled data
		predicate.and(qSupplier.enabled.eq(Boolean.TRUE));

		// find LIKE CustomerNumber
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getType())) {
			predicate.and(
					qSupplier.type.like("%" + supplierPaginationDTO.getParams().getType() + "%"));
		}

		// find LIKE customerName
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getName())) {
			StringTemplate convertedSupplierName = Expressions
					.stringTemplate("function('replace', {0}, '|', ' ')", qSupplier.name);
			predicate.and(convertedSupplierName
					.like("%" + supplierPaginationDTO.getParams().getName() + "%"));
		}

		// find by register number
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getRegisterNumber())
				&& ACMValidationUtils
						.isNullOrEmpty(supplierPaginationDTO.getParams().getIdentity())) {
			predicate.and(qSupplier.registerNumber
					.eq(supplierPaginationDTO.getParams().getRegisterNumber()));
		}
		// find by identity
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getIdentity())
				&& ACMValidationUtils
						.isNullOrEmpty(supplierPaginationDTO.getParams().getRegisterNumber())) {
			predicate.and(qSupplier.identity.eq(supplierPaginationDTO.getParams().getIdentity()));
		}

		// find by Identity or registerNumber
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getIdentity())
				&& !ACMValidationUtils
						.isNullOrEmpty(supplierPaginationDTO.getParams().getRegisterNumber())) {
			BooleanBuilder subPredicate = new BooleanBuilder();
			subPredicate.and(qSupplier.registerNumber
					.eq(supplierPaginationDTO.getParams().getRegisterNumber()));
			subPredicate.or(qSupplier.identity.eq(supplierPaginationDTO.getParams().getIdentity()));
			predicate.and(subPredicate);
		}

		// find not in RJECTED
		if (!ACMValidationUtils
				.isNullOrEmpty(supplierPaginationDTO.getParams().getStatusRejected())) {
			predicate.and(qSupplier.status
					.notEqualsIgnoreCase(supplierPaginationDTO.getParams().getStatusRejected()));
		}

		// find LIKE solidarityName
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getActivity())) {
			predicate.and(qSupplier.activity
					.like("%" + supplierPaginationDTO.getParams().getActivity() + "%"));
		}

		predicate.and(qSupplier.status.notEqualsIgnoreCase("NON CONTRACTED"));

		// find LIKE status
		if (!ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getParams().getStatus())) {
			predicate.and(qSupplier.status.eq(supplierPaginationDTO.getParams().getStatus()));
		}

		// find LIKE activity Name
		if (!ACMValidationUtils
				.isNullOrEmpty(supplierPaginationDTO.getParams().getActivityName())) {
			predicate.and(
					qSupplier.activityName.eq(supplierPaginationDTO.getParams().getActivityName()));
		}

		// find LIKE creation date
		// loanParams.applyDate
		if (!ACMValidationUtils
				.isNullOrEmpty(supplierPaginationDTO.getParams().getDateInsertion())) {
			java.sql.Date sqlDate = java.sql.Date.valueOf(DateUtil.convertToLocalDateViaInstant(
					supplierPaginationDTO.getParams().getDateInsertion()));
			predicate.and(qSupplier.dateInsertion.eq(sqlDate));
		}

		// init pageable params (page number / page size / sorting direction if exist)
		Pageable pageable = null;
		if ("1".equals(supplierPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = supplierPaginationDTO.getSortField();
			if (supplierPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(supplierPaginationDTO.getPageNumber(),
					supplierPaginationDTO.getPageSize(), Sort.Direction.ASC, sortedField);
		} // else
		else if ("-1".equals(supplierPaginationDTO.getSortDirection())
				&& !ACMValidationUtils.isNullOrEmpty(supplierPaginationDTO.getSortField())) {
			// customerNameNoPipe => customerName
			String sortedField = supplierPaginationDTO.getSortField();
			if (supplierPaginationDTO.getSortField().equals("customerNameNoPipe")) {
				sortedField = "customerName";
			}
			pageable = PageRequest.of(supplierPaginationDTO.getPageNumber(),
					supplierPaginationDTO.getPageSize(), Sort.Direction.DESC, sortedField);
		} // else
		else {
			// default sort by dateInsertion : DESC
			pageable = PageRequest.of(supplierPaginationDTO.getPageNumber(),
					supplierPaginationDTO.getPageSize(), Sort.Direction.DESC, "dateInsertion");
		}

		// load data
		Page<Supplier> pagedResult = supplierRepository.findAll(predicate, pageable);
		if (pagedResult.hasContent()) {
			List<Supplier> suppliers = pagedResult.getContent();
			List<SupplierDTO> supplierDTOs = new ArrayList<>();
			// mapping data
			suppliers.forEach(supplier -> {
				SupplierDTO dto = mapper.map(supplier, SupplierDTO.class);
				// get CA for supplier
				dto.setBalanceSupplier(findCABySupplier(supplier.getId()));
				supplierDTOs.add(dto);
			});

			logger.info("{} : Customer was founded (PageNumber = {} / PageSize = {} )",
					suppliers.size(), supplierPaginationDTO.getPageNumber(),
					supplierPaginationDTO.getPageSize());
			// setting data
			supplierPaginationDTO.setResultsSuppliers(supplierDTOs);
			// setting totals pages
			supplierPaginationDTO.setTotalElements(pagedResult.getTotalElements());
			// setting totals elements
			supplierPaginationDTO.setTotalPages(pagedResult.getTotalPages());
		}
		return supplierPaginationDTO;
	}

	/**
	 * Find CA by supplier.
	 *
	 * @param supplierId the supplier id
	 * @return the big decimal
	 */
	public BigDecimal findCABySupplier(Long supplierId) {

		logger.info("Calculate Supplier Loan Informations " + supplierId);
		Query q = entityManager.createNativeQuery(
				"select sum(coalesce(al.PRIX_UNITAIRE * (100-al.REMISE_ARTICLE) /100 * al.QUANTITE_ARTICLE, 0)) \r\n"
						+ "from ACM_ASSET_ACM_LOAN al join ACM_ASSET ass on ass.ID_ACM_ASSET = al.ID_ACM_ASSET join ACM_SUPPLIER sup on sup.ID_ACM_SUPPLIER = ass.SUPPLIER_ID\r\n"
						+ "where sup.ID_ACM_SUPPLIER =? ");
		q.setParameter(1, supplierId);

		List<BigDecimal> infos = (List<BigDecimal>) q.getResultList();
		if (!ACMValidationUtils.isNullOrEmpty(infos)) {
			return infos.get(0);
		}
		return BigDecimal.ZERO;
	}

	/**
	 * Find.
	 *
	 * @param supplierDTO the supplier DTO
	 * @return the list
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SupplierService#find(com.acm.utils.dtos.SupplierDTO)
	 */
	@Override
	public List<SupplierDTO> find(SupplierDTO supplierDTO) {

		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Save.
	 *
	 * @param id the id
	 * @param supplierDTO the supplier DTO
	 * @return the supplier DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SupplierService#save(java.lang.Long, com.acm.utils.dtos.SupplierDTO)
	 */
	@Override
	public SupplierDTO save(Long id, SupplierDTO supplierDTO) {

		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Find by id.
	 *
	 * @param id the id
	 * @return the supplier DTO
	 */
	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SupplierService#findById(java.lang.Long)
	 */
	@Override
	public SupplierDTO findById(Long id) {

		Optional<Supplier> supplier = supplierRepository.findById(id);
		SupplierDTO supplierDTO = mapper.map(supplier.get(), SupplierDTO.class);
		return supplierDTO;
	}

}
