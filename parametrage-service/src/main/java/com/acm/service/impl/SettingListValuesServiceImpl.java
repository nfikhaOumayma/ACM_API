/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.constants.common.CommonFunctions;
import com.acm.constants.common.CommonLoggerMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingListValuesRepository;
import com.acm.service.SettingListValuesService;
import com.acm.utils.dtos.ApplicationFeeDTO;
import com.acm.utils.dtos.AssetTypeListDTO;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.CollateralTypeDTO;
import com.acm.utils.dtos.DeferredPeriodTypeDTO;
import com.acm.utils.dtos.IndustryDTO;
import com.acm.utils.dtos.LoanGuarantorSourceDTO;
import com.acm.utils.dtos.LoanRefinanceReasonDTO;
import com.acm.utils.dtos.LoanSourceOfFundsDTO;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.ProductLoanReasonsDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.RoleAbacusDTO;
import com.acm.utils.dtos.SettingListValuesDTO;
import com.acm.utils.enums.SettingListValuesTable;
import com.acm.utils.models.QSettingListValues;
import com.acm.utils.models.SettingListValues;
import com.acm.utils.validation.ACMValidationUtils;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Preconditions;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingListValuesServiceImpl} Class Impl.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class SettingListValuesServiceImpl implements SettingListValuesService {

	/** The Constant logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingListValuesServiceImpl.class);

	/** The settingListValues repository. */
	@Autowired
	private SettingListValuesRepository settingListValuesRepository;

	/** The mapper. */
	@Autowired
	private DozerBeanMapper mapper;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/** The user Client client. */
	@Autowired
	private UserClient userClient;

	/** The setting transvers client. */
	@Autowired
	private TransversClient settingTransversClient;

	/** The entity manager. */
	@Autowired
	private EntityManager entityManager;

	/** The transaction manager. */
	@Autowired
	private PlatformTransactionManager transactionManager;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#find(java.lang.Long)
	 */
	@Override
	public SettingListValuesDTO find(Long id) throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		logger.info("Find SettingListValues by ID : {}", id);
		SettingListValues settingListValues = settingListValuesRepository.findById(id).orElse(null);
		// check if object is null
		if (ACMValidationUtils.isNullOrEmpty(settingListValues)) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingListValues.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					environment.getProperty("exception.message.not.found")
							+ SettingListValues.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		return mapper.map(settingListValues, SettingListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#find(com.acm.utils.dtos.SettingListValuesDTO)
	 */
	@Override
	public List<SettingListValuesDTO> find(SettingListValuesDTO settingListValuesDTO) {

		Preconditions.checkNotNull(settingListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_ID_NULL);

		// init QSettingListValues
		QSettingListValues qSettingListValues = QSettingListValues.settingListValues;
		// init Predicate
		BooleanBuilder predicate = new BooleanBuilder();

		// find only enabled data
		predicate.and(qSettingListValues.enabled.eq(Boolean.TRUE));
		// find by table name
		if (!ACMValidationUtils.isNullOrEmpty(settingListValuesDTO.getTableAbacusName())) {
			predicate.and(qSettingListValues.tableAbacusName
					.eq(settingListValuesDTO.getTableAbacusName()));
		}
		// find by listName
		if (!ACMValidationUtils.isNullOrEmpty(settingListValuesDTO.getListName())) {
			predicate.and(qSettingListValues.listName.eq(settingListValuesDTO.getListName()));
		}

		if (!ACMValidationUtils.isNullOrEmpty(settingListValuesDTO.getIdExtern())) {
			predicate.and(qSettingListValues.idExtern.eq(settingListValuesDTO.getIdExtern()));
		}

		Iterable<SettingListValues> iterable = settingListValuesRepository.findAll(predicate);
		List<SettingListValues> settingListValuess = new ArrayList<>();
		iterable.forEach(settingListValuess::add);
		logger.info("{} : SettingListValues was founded", settingListValuess.size());

		List<SettingListValuesDTO> settingListValuessDTOs = new ArrayList<>();
		settingListValuess.forEach(settingListValues -> settingListValuessDTOs
				.add(mapper.map(settingListValues, SettingListValuesDTO.class)));
		return settingListValuessDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#save(com.acm.utils.dtos.SettingListValuesDTO)
	 */
	@Override
	public SettingListValuesDTO save(SettingListValuesDTO settingListValuesDTO) {

		Preconditions.checkNotNull(settingListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		SettingListValues settingListValues =
				mapper.map(settingListValuesDTO, SettingListValues.class);
		CommonFunctions.mapperToSave(settingListValues, userClient, logger);
		SettingListValues newSettingListValues =
				settingListValuesRepository.save(settingListValues);

		logger.info(CommonLoggerMessage.SUCCESFULL_CREATE, SettingListValues.class.getSimpleName());
		return mapper.map(newSettingListValues, SettingListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#save(java.lang.Long,
	 * com.acm.utils.dtos.SettingListValuesDTO)
	 */
	@Override
	public SettingListValuesDTO save(Long id, SettingListValuesDTO settingListValuesDTO)
			throws ResourcesNotFoundException {

		Preconditions.checkNotNull(id, CommonExceptionsMessage.EXCEPTIONS_ID_NULL);
		Preconditions.checkNotNull(settingListValuesDTO,
				CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		logger.info("Update SettingListValues with ID = {}", id);
		SettingListValues oldSettingListValues =
				settingListValuesRepository.findById(id).orElse(null);

		// check if object is null
		if (oldSettingListValues == null) {
			logger.error(CommonLoggerMessage.NOT_FOUND_OBJECT,
					SettingListValues.class.getSimpleName());
			throw new ResourcesNotFoundException(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL,
					CommonExceptionsMessage.NOT_FOUND + SettingListValues.class.getSimpleName()
							+ CommonExceptionsMessage.WITH_ID + id);
		}
		// mapping new data with existing data (oldSettingListValues)
		mapper.map(settingListValuesDTO, oldSettingListValues);
		CommonFunctions.mapperToUpdate(oldSettingListValues, userClient, logger);

		// update & persist data in DB
		SettingListValues newSettingListValues =
				settingListValuesRepository.save(oldSettingListValues);

		logger.info(CommonLoggerMessage.SUCCESFULL_UPDATE, SettingListValues.class.getSimpleName());
		return mapper.map(newSettingListValues, SettingListValuesDTO.class);
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#findAllPortfolio()
	 */
	@Override
	public List<PortfolioDTO> findAllPortfolio() {

		return settingTransversClient.findAllPortfolio();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#loadSettingFromAbacus()
	 */
	@Override
	public void loadSettingFromAbacus() {

		List<BrancheDTO> brancheDTOs = settingTransversClient.findBranches();
		for (BrancheDTO data : brancheDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.BRANCHES.tableName(), String.valueOf(data.getBranchID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		// execute stored procedure : Update the old branch descriptions for existing
		// customers.
		Integer numberRows = settingListValuesRepository.updateBranchesDescription();
		logger.info("Number of updated rows = {}", numberRows);
		logger.info("PROCEDURE UPDATE BRANCH NAME AND DESCRIPTION :: DONE");
		logger.info("############ Branche :: INSERT DONE ############");

		List<ProductLoanReasonsDTO> productLoanReasonsDTOs =
				settingTransversClient.findProductLoanReasons();
		for (ProductLoanReasonsDTO data : productLoanReasonsDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.CU_PRODUCT_LOAN_REASONS.tableName(),
					String.valueOf(data.getLoanReasonID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ ProductLoanReasons :: INSERT DONE  ############");

		List<LoanGuarantorSourceDTO> loanGuarantorSourceDTOs =
				settingTransversClient.findLoanGuarantorSource();
		for (LoanGuarantorSourceDTO data : loanGuarantorSourceDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.CU_LOAN_GUARANTOR_SOURCE.tableName(),
					String.valueOf(data.getLoanGuarantorSourceID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ LoanGuarantorSource :: INSERT DONE  ############");

		List<LoanSourceOfFundsDTO> loanSourceOfFundsDTOs =
				settingTransversClient.findLoanSourceOfFunds();
		for (LoanSourceOfFundsDTO data : loanSourceOfFundsDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.CU_LOAN_SOURCE_OF_FUNDS.tableName(),
					String.valueOf(data.getLoanSourceOfFundsID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ LoanSourceOfFunds :: INSERT DONE  ############");

		List<LoanRefinanceReasonDTO> loanRefinanceReasonDTOs =
				settingTransversClient.findLoanRefinanceReason();
		for (LoanRefinanceReasonDTO data : loanRefinanceReasonDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.CU_LOAN_REFINANCE_REASON.tableName(),
					String.valueOf(data.getLoanRefinanceReasonID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ LoanRefinanceReason :: INSERT DONE  ############");

		List<RelationshipDTO> relationshipDTOs = settingTransversClient.findRelationship();
		for (RelationshipDTO data : relationshipDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO =
					new SettingListValuesDTO(SettingListValuesTable.RELATIONSHIP.tableName(),
							String.valueOf(data.getRelationshipID()),
							CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ Relationship :: INSERT DONE  ############");

		List<IndustryDTO> industryDTOs = settingTransversClient.findIndustry();
		for (IndustryDTO data : industryDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.SECTOR.tableName(), String.valueOf(data.getIndustryID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ Industry :: INSERT DONE  ############");

		List<RoleAbacusDTO> roleAbacusDTOs = settingTransversClient.findRoleAbacus();
		for (RoleAbacusDTO data : roleAbacusDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.CU_ROLE.tableName(), String.valueOf(data.getRoleID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ RoleAbacus :: INSERT DONE  ############");

		List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs =
				settingTransversClient.findDeferredPeriodType();
		for (DeferredPeriodTypeDTO data : deferredPeriodTypeDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.DEFERRED_PERIOD_TYPE.tableName(),
					String.valueOf(data.getDeferredPeriodTypeId()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ DeferredPeriodType :: INSERT DONE  ############");

		List<CollateralTypeDTO> collateralTypeDTOs = settingTransversClient.findCollateralTypes();
		for (CollateralTypeDTO data : collateralTypeDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO =
					new SettingListValuesDTO(SettingListValuesTable.CU_COLLATERAL_TYPE.tableName(),
							String.valueOf(data.getId()),
							CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ RoleAbacus :: INSERT DONE  ############");

		List<ApplicationFeeDTO> applicationFeeDTOs = settingTransversClient.findFees();
		for (ApplicationFeeDTO data : applicationFeeDTOs) {
			logger.debug("{}", data);
			SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO(
					SettingListValuesTable.FEES.tableName(), String.valueOf(data.getCufeeID()),
					CommonFunctions.convertObjectToJSONString(data), null);
			logger.debug("{}", settingListValuesDTO);
			save(settingListValuesDTO);
		}
		logger.info("############ fees :: INSERT DONE  ############");

	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#resetSettingFromAbacus()
	 */
	@Override
	public void resetSettingFromAbacus() {

		// Get acm setting list values
		SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO();
		settingListValuesDTO.setIdExtern("-1");
		settingListValuesDTO.setTableAbacusName(null);

		List<SettingListValuesDTO> acmSettingListValues = find(settingListValuesDTO);
		try {
			logger.info("Method settingListValues : resetSettingFromAbacus() :: Start");
			// remove All data from DB
			settingListValuesRepository.deleteAll();

			// Reset Primary Key Value to "1"
			int status = resetPrimaryKey();
			logger.info("Reset Primary Key Value :: DONE with status = {} ", status);

			// reload ADDRESS setting
			loadSettingFromAbacus();

			// Save acm setting list values
			acmSettingListValues.forEach(settingListValue -> {
				settingListValue.setId(null);
				save(settingListValue);
			});
		}
		catch (Exception e) {
			// Save acm setting list values
			acmSettingListValues.forEach(settingListValue -> {
				settingListValue.setId(null);
				save(settingListValue);
			});
		}

		logger.info("Method settingListValues : resetSettingFromAbacus() :: DONE");

	}

	/**
	 * Reset primary key to start from : "1".
	 * 
	 * @author HaythemBenizid
	 * @return the integer
	 */
	public Integer resetPrimaryKey() {

		TransactionTemplate transactionTemplate = new TransactionTemplate(transactionManager);
		return transactionTemplate.execute(transactionStatus -> {
			int status = entityManager
					.createNativeQuery("DBCC CHECKIDENT (ACM_SETTING_LIST_VALUES, RESEED, 0)")
					.executeUpdate();
			transactionStatus.flush();
			return status;
		});
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#findDeferredPeriodTypes()
	 */
	@Override
	public List<DeferredPeriodTypeDTO> findDeferredPeriodTypes() {

		List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = find(
				new SettingListValuesDTO(SettingListValuesTable.DEFERRED_PERIOD_TYPE.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> deferredPeriodTypeDTOs
				.add((DeferredPeriodTypeDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), DeferredPeriodTypeDTO.class)));
		return deferredPeriodTypeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#findFees()
	 */
	@Override
	public List<ApplicationFeeDTO> findFees() {

		List<ApplicationFeeDTO> applicationFeeDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs =
				find(new SettingListValuesDTO(SettingListValuesTable.FEES.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> applicationFeeDTOs
				.add((ApplicationFeeDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), ApplicationFeeDTO.class)));
		return applicationFeeDTOs;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesService#convertJsonToArray(java.lang.String)
	 */
	@Override
	public List<AssetTypeListDTO> convertJsonToArray(String jsonValue) {

		ObjectMapper objectMapper = new ObjectMapper();
		List<AssetTypeListDTO> assetTypeListDTOss = new ArrayList<>();
		try {
			Map<String, AssetTypeListDTO> assetTypeListDTOs = objectMapper.readValue(jsonValue,
					new TypeReference<Map<String, AssetTypeListDTO>>() {

					});

			for (Map.Entry<String, AssetTypeListDTO> entry : assetTypeListDTOs.entrySet()) {
				AssetTypeListDTO assetTypeListDTO = entry.getValue();
				assetTypeListDTOss.add(assetTypeListDTO);
			}

		}
		catch (IOException e) {
			e.printStackTrace();
		}

		return assetTypeListDTOss;
	}

}
