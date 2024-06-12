/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service.impl;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;
import org.springframework.stereotype.Service;

import com.acm.constants.common.CommonLoggerMessage;
import com.acm.model.rowmapper.BrancheDTOAbacusRowMapper;
import com.acm.model.rowmapper.CollateralTypeDTOAbacusRowMapper;
import com.acm.model.rowmapper.DeferredPeriodTypeAbacusRowMapper;
import com.acm.model.rowmapper.IndustryAbacusRowMapper;
import com.acm.model.rowmapper.LoanDistrictCodeAbacusRowMapper;
import com.acm.model.rowmapper.LoanGuarantorSourceAbacusRowMapper;
import com.acm.model.rowmapper.LoanRefinanceReasonAbacusRowMapper;
import com.acm.model.rowmapper.LoanSourceOfFundsAbacusRowMapper;
import com.acm.model.rowmapper.ProductLoanReasonsAbacusRowMapper;
import com.acm.model.rowmapper.RelationshipAbacusRowMapper;
import com.acm.model.rowmapper.RoleAbacusRowMapper;
import com.acm.model.rowmapper.SettingListValuesRowMapper;
import com.acm.service.SettingListValuesAbacusService;
import com.acm.utils.dtos.AccountAbacusDTO;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.CollateralTypeDTO;
import com.acm.utils.dtos.DeferredPeriodTypeDTO;
import com.acm.utils.dtos.IndustryDTO;
import com.acm.utils.dtos.LoanDistrictCodeDTO;
import com.acm.utils.dtos.LoanGuarantorSourceDTO;
import com.acm.utils.dtos.LoanRefinanceReasonDTO;
import com.acm.utils.dtos.LoanSourceOfFundsDTO;
import com.acm.utils.dtos.ProductLoanReasonsDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.RoleAbacusDTO;
import com.acm.utils.dtos.SettingListValuesDTO;

/**
 * {@link SettingListValuesAbacusServiceImpl} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@Service
public class SettingListValuesAbacusServiceImpl implements SettingListValuesAbacusService {

	/** logger. */
	private static final Logger logger =
			LoggerFactory.getLogger(SettingListValuesAbacusServiceImpl.class);

	/** The named parameter jdbc template. */
	@Autowired
	private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

	/** The Environment. */
	@Autowired
	private Environment environment;

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findBranches()
	 */
	@Override
	public List<BrancheDTO> findBranches() {

		try {
			// load query
			String query = environment.getProperty("query.select.branches");

			// execute query
			List<BrancheDTO> brancheDTOs =
					namedParameterJdbcTemplate.query(query, new BrancheDTOAbacusRowMapper());
			logger.info(" find BRANCHES data from ABACUS DB :: DONE");
			// returning result
			return brancheDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findProductLoanReasons()
	 */
	@Override
	public List<ProductLoanReasonsDTO> findProductLoanReasons() {

		try {
			// load query
			String query = environment
					.getProperty("query.select.settings.listValues.CUProductLoanReasons");

			// execute query
			List<ProductLoanReasonsDTO> productLoanReasonsDTOs = namedParameterJdbcTemplate
					.query(query, new ProductLoanReasonsAbacusRowMapper());
			logger.info(" find PRODUCT_LOAN_REASONS data from ABACUS DB :: DONE");
			// returning result
			return productLoanReasonsDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findLoanGuarantorSource()
	 */
	@Override
	public List<LoanGuarantorSourceDTO> findLoanGuarantorSource() {

		try {
			// load query
			String query = environment
					.getProperty("query.select.settings.listValues.CULoanGuarantorSource");

			// execute query
			List<LoanGuarantorSourceDTO> loanGuarantorSourceDTOs = namedParameterJdbcTemplate
					.query(query, new LoanGuarantorSourceAbacusRowMapper());
			logger.info(" find LOANGUARANTORSOURCE data from ABACUS DB :: DONE");
			// returning result
			return loanGuarantorSourceDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findLoanSourceOfFunds()
	 */
	@Override
	public List<LoanSourceOfFundsDTO> findLoanSourceOfFunds() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.settings.listValues.CULoanSourceOfFunds");

			// execute query
			List<LoanSourceOfFundsDTO> loanSourceOfFundsDTOs =
					namedParameterJdbcTemplate.query(query, new LoanSourceOfFundsAbacusRowMapper());
			logger.info(" find LoanSourceOfFunds data from ABACUS DB :: DONE");
			// returning result
			return loanSourceOfFundsDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findLoanRefinanceReason()
	 */
	@Override
	public List<LoanRefinanceReasonDTO> findLoanRefinanceReason() {

		try {
			// load query
			String query = environment
					.getProperty("query.select.settings.listValues.CULoanRefinanceReason");

			// execute query
			List<LoanRefinanceReasonDTO> loanRefinanceReasonDTOs = namedParameterJdbcTemplate
					.query(query, new LoanRefinanceReasonAbacusRowMapper());
			logger.info(" find LoanRefinanceReason data from ABACUS DB :: DONE");
			// returning result
			return loanRefinanceReasonDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findRelationship()
	 */
	@Override
	public List<RelationshipDTO> findRelationship() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.listValues.Relationship");

			// execute query
			List<RelationshipDTO> relationshipDTOs =
					namedParameterJdbcTemplate.query(query, new RelationshipAbacusRowMapper());
			logger.info(" find Relationship data from ABACUS DB :: DONE");
			// returning result
			return relationshipDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findIndustry()
	 */
	@Override
	public List<IndustryDTO> findIndustry() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.listValues.Industry");

			// execute query
			List<IndustryDTO> industryDTOs =
					namedParameterJdbcTemplate.query(query, new IndustryAbacusRowMapper());
			logger.info(" find Industry data from ABACUS DB :: DONE");
			// returning result
			return industryDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findRoleAbacus()
	 */
	@Override
	public List<RoleAbacusDTO> findRoleAbacus() {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.listValues.CURole");

			// execute query
			List<RoleAbacusDTO> roleAbacusDTOs =
					namedParameterJdbcTemplate.query(query, new RoleAbacusRowMapper());
			logger.info(" find RoleAbacus data from ABACUS DB :: DONE");
			// returning result
			return roleAbacusDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findDistricCode()
	 */
	@Override
	public List<LoanDistrictCodeDTO> findDistricCode() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.settings.listValues.CULoanDistrictCode");

			// execute query
			List<LoanDistrictCodeDTO> districtCodeAbacusDTOs =
					namedParameterJdbcTemplate.query(query, new LoanDistrictCodeAbacusRowMapper());
			logger.info(" find RoleAbacus data from ABACUS DB :: DONE");
			// returning result
			return districtCodeAbacusDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findDeferredPeriodType()
	 */
	@Override
	public List<DeferredPeriodTypeDTO> findDeferredPeriodType() {

		try {
			// load query
			String query =
					environment.getProperty("query.select.settings.listValues.DeferredOptions");

			// execute query
			List<DeferredPeriodTypeDTO> deferredPeriodTypeDTOs = namedParameterJdbcTemplate
					.query(query, new DeferredPeriodTypeAbacusRowMapper());
			logger.info(" find RoleAbacus data from ABACUS DB :: DONE");
			// returning result
			return deferredPeriodTypeDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findCollateralTypes()
	 */
	@Override
	public List<CollateralTypeDTO> findCollateralTypes() {

		try {
			// load query
			String query = environment
					.getProperty("query.select.settings.listValues.CULoanCollateralType");

			// execute query
			List<CollateralTypeDTO> collateralTypeDTOs =
					namedParameterJdbcTemplate.query(query, new CollateralTypeDTOAbacusRowMapper());
			logger.info(" find collateralType data from ABACUS DB :: DONE");
			// returning result
			return collateralTypeDTOs;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findCreditAccount(java.lang.String)
	 */
	@Override
	public List<AccountAbacusDTO> findCreditAccount(String account) {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.creditAcount");
			// execute query
			SqlParameterSource namedParameters =
					new MapSqlParameterSource().addValue("account", "%" + account + "%");
			// execute query
			return namedParameterJdbcTemplate
					.query(query, namedParameters, new RowMapper<AccountAbacusDTO>() {
						@Override
						public AccountAbacusDTO mapRow(ResultSet rs, int rowNum)
								throws SQLException {

							AccountAbacusDTO accountAbacusDTO = new AccountAbacusDTO();
							accountAbacusDTO.setNumber(rs.getString("number"));
							accountAbacusDTO.setAccountID(rs.getLong("AccountID"));

							return accountAbacusDTO;
						}
					}).stream().collect(Collectors.toList());
			// returning result

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findMainAccount(java.lang.Long,
	 * java.lang.Integer)
	 */
	@Override
	public Long findMainAccount(Long idAccount, Integer branchId) {

		try {
			// load query
			String query = environment.getProperty("query.select.settings.mainAcount");
			// execute query
			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("accountId", idAccount).addValue("branchId", branchId);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters, new RowMapper<Long>() {
				@Override
				public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

					return rs.getLong("AccountID");
				}
			}).get(0);
			// returning result

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findAccountId(java.lang.Integer,
	 * java.lang.String)
	 */
	@Override
	public Long findAccountId(Integer branchId, String number) {

		try {
			// load query
			String query = environment.getProperty("query.select.get.AccountId");
			// execute query
			SqlParameterSource namedParameters = new MapSqlParameterSource()
					.addValue("number", number).addValue("branchId", branchId);
			// execute query
			return namedParameterJdbcTemplate.query(query, namedParameters, new RowMapper<Long>() {
				@Override
				public Long mapRow(ResultSet rs, int rowNum) throws SQLException {

					return rs.getLong("AccountID");
				}
			}).get(0);
			// returning result

		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see com.acm.service.SettingListValuesAbacusService#findJournals()
	 */
	@Override
	public List<SettingListValuesDTO> findJournals() {

		try {
			// load query
			String query = environment.getProperty("query.select.get.journals");

			// execute query
			List<SettingListValuesDTO> journals =
					namedParameterJdbcTemplate.query(query, new SettingListValuesRowMapper());
			logger.info(" find journals data from ABACUS DB :: DONE");
			// returning result
			return journals;
		}
		catch (Exception e) {
			logger.error(CommonLoggerMessage.ERROR_WHILE_FINDING_DATA_FROM_ABACUS);
			logger.error("{}", e.getMessage());
		}
		return new ArrayList<>();

	}

}
