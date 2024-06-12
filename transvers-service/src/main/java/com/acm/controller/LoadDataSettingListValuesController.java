/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
 * This class @{link LoadDataSettingListValuesController}.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataSettingListValuesController {

	/** The setting list values abacus service. */
	@Autowired
	private SettingListValuesAbacusService settingListValuesAbacusService;

	/**
	 * Find branch.
	 * 
	 * @author YesserSomai
	 * @return the branche DTO
	 */
	@GetMapping("/setting/branche/")
	public List<BrancheDTO> findBranches() {

		return settingListValuesAbacusService.findBranches();
	}

	/**
	 * Find product loan reasons.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-product-loan-reasons/")
	public List<ProductLoanReasonsDTO> findProductLoanReasons() {

		return settingListValuesAbacusService.findProductLoanReasons();
	}

	/**
	 * Find loan guarantor source.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-loan-guarantor-source/")
	public List<LoanGuarantorSourceDTO> findLoanGuarantorSource() {

		return settingListValuesAbacusService.findLoanGuarantorSource();
	}

	/**
	 * Find loan source of funds.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-loan-source-of-funds/")
	public List<LoanSourceOfFundsDTO> findLoanSourceOfFunds() {

		return settingListValuesAbacusService.findLoanSourceOfFunds();
	}

	/**
	 * Find loan refinance reason.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-loan-refinance-reason/")
	public List<LoanRefinanceReasonDTO> findLoanRefinanceReason() {

		return settingListValuesAbacusService.findLoanRefinanceReason();
	}

	/**
	 * Find relationship.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-relationship/")
	public List<RelationshipDTO> findRelationship() {

		return settingListValuesAbacusService.findRelationship();
	}

	/**
	 * Find industry.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-industry/")
	public List<IndustryDTO> findIndustry() {

		return settingListValuesAbacusService.findIndustry();
	}

	/**
	 * Find role ABACUS.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/setting/find-role-abacus/")
	public List<RoleAbacusDTO> findRoleAbacus() {

		return settingListValuesAbacusService.findRoleAbacus();
	}

	/**
	 * Find district code.
	 *
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/setting/find-district-code/")
	public List<LoanDistrictCodeDTO> findDisctrictCode() {

		return settingListValuesAbacusService.findDistricCode();
	}

	/**
	 * Find deferred period type DTO.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/setting/find-deferredPeriod-type-abacus/")
	public List<DeferredPeriodTypeDTO> findDeferredPeriodType() {

		return settingListValuesAbacusService.findDeferredPeriodType();
	}

	/**
	 * Find collateral types.
	 * 
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/setting/find-collateral-types/")
	public List<CollateralTypeDTO> findCollateralTypes() {

		return settingListValuesAbacusService.findCollateralTypes();
	}

	/**
	 * Find credit acount.
	 *
	 * @param account the account
	 * @return the list
	 */
	@PostMapping("/find_credit_acount")
	public List<AccountAbacusDTO> findCreditAcount(@RequestBody String account) {

		return settingListValuesAbacusService.findCreditAccount(account);
	}

	/**
	 * Find main account.
	 *
	 * @param accountId the account id
	 * @param branchId the branch id
	 * @return the long
	 */
	@GetMapping("/find-main-account/{accountId}/{branchId}")
	public Long findMainAccount(@PathVariable("accountId") Long accountId,
			@PathVariable("branchId") Integer branchId) {

		return settingListValuesAbacusService.findMainAccount(accountId, branchId);
	}

	/**
	 * Find account id.
	 *
	 * @param branchId the branch id
	 * @param number the number
	 * @return the long
	 */
	@PostMapping("/find-account-id/{branchId}")
	public Long findAccountId(@PathVariable("branchId") Integer branchId,
			@RequestBody String number) {

		return settingListValuesAbacusService.findAccountId(branchId, number);
	}

	/**
	 * Find journals.
	 *
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/find-journals")
	public List<SettingListValuesDTO> findJournals() {

		return settingListValuesAbacusService.findJournals();
	}
}
