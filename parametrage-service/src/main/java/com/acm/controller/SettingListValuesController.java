/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonFunctions;
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

/**
 * This class @{link SettingListValuesController} used to control all the
 * {@link SettingListValuesDTO} requests.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/settings-list-values")
public class SettingListValuesController {

	/** The setting list values service. */
	@Autowired
	private SettingListValuesService settingListValuesService;

	/**
	 * Find loan guarantor source.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-loan-guarantor-source")
	public List<LoanGuarantorSourceDTO> findLoanGuarantorSource() {

		List<LoanGuarantorSourceDTO> loanGuarantorSourceDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(new SettingListValuesDTO(
						SettingListValuesTable.CU_LOAN_GUARANTOR_SOURCE.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> loanGuarantorSourceDTOs
				.add((LoanGuarantorSourceDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), LoanGuarantorSourceDTO.class)));
		return loanGuarantorSourceDTOs;
	}

	/**
	 * Find product loan reasons.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-product-loan-leasons")
	public List<ProductLoanReasonsDTO> findProductLoanReasons() {

		List<ProductLoanReasonsDTO> productLoanReasonsDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(new SettingListValuesDTO(
						SettingListValuesTable.CU_PRODUCT_LOAN_REASONS.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> productLoanReasonsDTOs
				.add((ProductLoanReasonsDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), ProductLoanReasonsDTO.class)));
		return productLoanReasonsDTOs;
	}

	/**
	 * Find loan source of funds.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-loan-source-funds")
	public List<LoanSourceOfFundsDTO> findLoanSourceOfFunds() {

		List<LoanSourceOfFundsDTO> loanSourceOfFundsDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(new SettingListValuesDTO(
						SettingListValuesTable.CU_LOAN_SOURCE_OF_FUNDS.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> loanSourceOfFundsDTOs
				.add((LoanSourceOfFundsDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), LoanSourceOfFundsDTO.class)));
		return loanSourceOfFundsDTOs;
	}

	/**
	 * Find loan refinance reason.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-loan-refinance-reason")
	public List<LoanRefinanceReasonDTO> findLoanRefinanceReason() {

		List<LoanRefinanceReasonDTO> loanRefinanceReasonDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(new SettingListValuesDTO(
						SettingListValuesTable.CU_LOAN_REFINANCE_REASON.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> loanRefinanceReasonDTOs
				.add((LoanRefinanceReasonDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), LoanRefinanceReasonDTO.class)));
		return loanRefinanceReasonDTOs;
	}

	/**
	 * Find branches.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-branches")
	public List<BrancheDTO> findBranches() {

		List<BrancheDTO> brancheDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = settingListValuesService
				.find(new SettingListValuesDTO(SettingListValuesTable.BRANCHES.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> brancheDTOs
				.add((BrancheDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), BrancheDTO.class)));
		return brancheDTOs;
	}

	/**
	 * Find relationship.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-relationships")
	public List<RelationshipDTO> findRelationship() {

		List<RelationshipDTO> relationshipDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = settingListValuesService
				.find(new SettingListValuesDTO(SettingListValuesTable.RELATIONSHIP.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> relationshipDTOs
				.add((RelationshipDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), RelationshipDTO.class)));
		return relationshipDTOs;
	}

	/**
	 * Find industry.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-industrys")
	public List<IndustryDTO> findIndustry() {

		List<IndustryDTO> industryDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = settingListValuesService
				.find(new SettingListValuesDTO(SettingListValuesTable.SECTOR.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> industryDTOs
				.add((IndustryDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), IndustryDTO.class)));
		return industryDTOs;
	}

	/**
	 * Find role abacus.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-roles-abacus")
	public List<RoleAbacusDTO> findRoleAbacus() {

		List<RoleAbacusDTO> roleAbacusDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = settingListValuesService
				.find(new SettingListValuesDTO(SettingListValuesTable.CU_ROLE.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> roleAbacusDTOs
				.add((RoleAbacusDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), RoleAbacusDTO.class)));
		return roleAbacusDTOs;
	}

	/**
	 * Find all portfolios.
	 *
	 * @author Salmen Fatnassi
	 * @return the list of Portfolio DTO
	 */
	@GetMapping("/portfolio")
	public List<PortfolioDTO> findAll() {

		return settingListValuesService.findAllPortfolio();
	}

	/**
	 * Load setting from abacus (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 */
	@GetMapping("/load-setting")
	public void loadSettingFromAbacus() {

		settingListValuesService.loadSettingFromAbacus();
	}

	/**
	 * Reset setting list value from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @return the response entity OK
	 */
	@GetMapping("/reset-setting")
	public String resetSettingFromAbacus() {

		settingListValuesService.resetSettingFromAbacus();
		return JSONObject.quote("Reset  List Values DONE !!");
	}

	/**
	 * Find deferred period type.
	 * 
	 * @author mlamloum
	 * @return the list
	 */
	@GetMapping("/find-deferred-period-type")
	public List<DeferredPeriodTypeDTO> findDeferredPeriodTypes() {

		return settingListValuesService.findDeferredPeriodTypes();
	}

	/**
	 * Find fees.
	 *
	 * @return the list
	 */
	@GetMapping("/find-fees")
	public List<ApplicationFeeDTO> findFees() {

		return settingListValuesService.findFees();

	}

	/**
	 * Find collateral types.
	 * 
	 * @author mlamloum
	 * @return the list
	 */
	@PostMapping("/find-collateral-types")
	public List<CollateralTypeDTO> findCollateralTypes() {

		List<CollateralTypeDTO> collateralTypeDTOs = new ArrayList<>();
		List<SettingListValuesDTO> settingListValuesDTOs = settingListValuesService.find(
				new SettingListValuesDTO(SettingListValuesTable.CU_COLLATERAL_TYPE.tableName()));
		settingListValuesDTOs.forEach(settingListValue -> collateralTypeDTOs
				.add((CollateralTypeDTO) CommonFunctions.convertJSONStringtoObject(
						settingListValue.getValueJson(), CollateralTypeDTO.class)));

		return collateralTypeDTOs.stream().filter(s -> s.getEnabled().equals(Boolean.TRUE))
				.collect(Collectors.toList());
	}

	/**
	 * Find asset type list.
	 *
	 * @param listName the list name
	 * @return the list
	 */
	@GetMapping("/find-asset-list/{listName}")
	public List<AssetTypeListDTO> findAssetTypeList(@PathVariable String listName) {

		List<AssetTypeListDTO> assetTypeListDTOs = new ArrayList<>();
		SettingListValuesDTO settingListValuesDTOnew = new SettingListValuesDTO();
		settingListValuesDTOnew.setListName(listName);
		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(settingListValuesDTOnew);
		if (settingListValuesDTOs.size() > 0) {
			assetTypeListDTOs = settingListValuesService
					.convertJsonToArray(settingListValuesDTOs.get(0).getValueJson());
		}
		return assetTypeListDTOs;
	}

	/**
	 * Find by list name.
	 *
	 * @param settingListValueDTO the setting list value DTO
	 * @return the list
	 */
	@PostMapping("/find-list-value")
	public List<SettingListValuesDTO> findByListName(
			@RequestBody SettingListValuesDTO settingListValueDTO) {

		List<SettingListValuesDTO> settingListValuesDTOs =
				settingListValuesService.find(settingListValueDTO);

		return settingListValuesDTOs;
	}
}
