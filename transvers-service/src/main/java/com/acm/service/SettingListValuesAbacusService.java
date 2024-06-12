/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import java.util.List;

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
 * {@link SettingListValuesAbacusService} interface.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public interface SettingListValuesAbacusService {

	/**
	 * Find {@link List} of {@link BrancheDTO} data.
	 *
	 * @author HaythemBenizid
	 * @return the list branche DTO
	 */
	List<BrancheDTO> findBranches();

	/**
	 * Find product loan reasons.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<ProductLoanReasonsDTO> findProductLoanReasons();

	/**
	 * Find loan guarantor source.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<LoanGuarantorSourceDTO> findLoanGuarantorSource();

	/**
	 * Find loan source of funds.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<LoanSourceOfFundsDTO> findLoanSourceOfFunds();

	/**
	 * Find loan refinance reason.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<LoanRefinanceReasonDTO> findLoanRefinanceReason();

	/**
	 * Find relationship.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<RelationshipDTO> findRelationship();

	/**
	 * Find industry.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<IndustryDTO> findIndustry();

	/**
	 * Find role ABACUS.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	List<RoleAbacusDTO> findRoleAbacus();

	/**
	 * Find district code.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */

	List<LoanDistrictCodeDTO> findDistricCode();

	/**
	 * Find deferred period type.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	List<DeferredPeriodTypeDTO> findDeferredPeriodType();

	/**
	 * Find collateral types.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	List<CollateralTypeDTO> findCollateralTypes();

	/**
	 * Find credit account.
	 *
	 * @param creditAcount the credit acount
	 * @return the list
	 */
	List<AccountAbacusDTO> findCreditAccount(String creditAcount);

	/**
	 * Find main account.
	 *
	 * @author kouali
	 * @param accountId the account id
	 * @param branchId the branch id
	 * @return the long
	 */
	Long findMainAccount(Long accountId, Integer branchId);

	/**
	 * Find account id.
	 *
	 * @param branchId the branch id
	 * @param number the number
	 * @return the long
	 */
	Long findAccountId(Integer branchId, String number);

	/**
	 * Find journals.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	List<SettingListValuesDTO> findJournals();
}
