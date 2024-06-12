/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.CollateralAbacusService;
import com.acm.service.GuarantorAbacusService;
import com.acm.service.LoanAbacusService;
import com.acm.service.LoanProcessSettingAbacusService;
import com.acm.utils.dtos.AcmCollateralDTO;
import com.acm.utils.dtos.CollaterolDTO;
import com.acm.utils.dtos.GuarantorDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoanProcessSettingDTO;
import com.acm.utils.dtos.ScheduleDTO;
import com.acm.utils.dtos.SettingMotifRejetsDTO;
import com.acm.utils.dtos.SettingTopupValidityDTO;

/**
 * This class @{link LoadDataCustomerController}.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataLoanController {

	/** The loan abacus service. */
	@Autowired
	private LoanAbacusService loanAbacusService;

	/** The guarantor abacus service. */
	@Autowired
	private GuarantorAbacusService guarantorAbacusService;

	/** The collateral abacus service. */
	@Autowired
	private CollateralAbacusService collateralAbacusService;

	/** The loan process abacus service. */
	@Autowired
	private LoanProcessSettingAbacusService loanProcessAbacusService;

	/**
	 * Find loan from abacus DB by given params.
	 *
	 * @author HaythemBenizid
	 * @param token the token
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/loans/{limite}")
	public List<LoanDTO> findLoans(@RequestHeader("Authorization") String token,
			@PathVariable("limite") Long limite) {

		return loanAbacusService.find(limite);
	}

	/**
	 * Find loan by account number.
	 * 
	 * @author idridi
	 * @param accountNumber the account number
	 * @return the list
	 */
	@GetMapping("/get-loan-by-accountNumber/{accountNumber}")
	public List<LoanDTO> findLoanByAccountNumber(
			@PathVariable("accountNumber") String accountNumber) {

		return loanAbacusService.findLoanByAccountNumber(accountNumber);
	}

	/**
	 * Find issued loansby id externe.
	 * 
	 * @author idridi
	 * @param ids the ids
	 * @return the list
	 */
	@PostMapping("/find-loans-issued")
	public List<LoanDTO> findIssuedLoansbyIdExterne(@RequestBody List<Long> ids) {

		return loanAbacusService.findIssuedLoans(ids);
	}

	/**
	 * Find loan by ID.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the loan DTO selected by idloan
	 */
	@GetMapping("/loan/{idLoan}")
	public LoanDTO findLoan(@PathVariable("idLoan") Long idLoan) {

		return loanAbacusService.findDetails(idLoan);
	}

	/**
	 * Find list of guarantors by given loan ID.
	 * 
	 * @author HaythemBenizid
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/loans/guarantors/{idLoan}")
	public List<GuarantorDTO> findGuarantors(@PathVariable("idLoan") Long idLoan) {

		return guarantorAbacusService.find(idLoan);
	}

	/**
	 * Find list of collaterol by given loan ID.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list of Collaterols
	 */
	@GetMapping("/loans/collaterols/{idLoan}")
	public List<CollaterolDTO> findCollaterols(@PathVariable("idLoan") Long idLoan) {

		return collateralAbacusService.find(idLoan);
	}

	/**
	 * Find active and inactive collaterals.
	 *
	 * @author mlamloum
	 * @param idLoans the id loans
	 * @return the list
	 */
	@PostMapping("/loans/all-collaterols")
	public List<AcmCollateralDTO> findActiveAndInactiveCollaterols(
			@RequestBody List<Long> idLoans) {

		return collateralAbacusService.findActiveAndInactiveCollaterols(idLoans);
	}

	/**
	 * Find schedule.
	 * 
	 * @author YesserSomai
	 * @param idLoan the id loan
	 * @return the list
	 */
	@GetMapping("/loans/schedule/{idLoan}")
	public List<ScheduleDTO> findSchedules(@PathVariable("idLoan") Long idLoan) {

		return loanAbacusService.findSchedule(idLoan);
	}

	/**
	 * Find Loan Process by Product.
	 *
	 * @author RadhouaneHomrani
	 * @param idProduct the id product
	 * @return the loan process DTO
	 */
	@GetMapping("/loans/process/{idProduct}")
	public List<LoanProcessSettingDTO> findLoanProcessSetting(
			@PathVariable("idProduct") Long idProduct) {

		return loanProcessAbacusService.find(idProduct);
	}

	/**
	 * Find all motif rejet.
	 * 
	 * @author ManelLamloum
	 * @return the list
	 */
	@GetMapping("/loans/find-all-motif-rejet")
	public List<SettingMotifRejetsDTO> findAllMotifRejet() {

		return loanAbacusService.findAllMotifRejet();
	}

	/**
	 * Find canceled loan.
	 *
	 * @author YesserSomai
	 * @return the list
	 */
	@GetMapping("/loans/find-canceled-loan")
	public List<LoanDTO> findCanceledLoan() {

		return loanAbacusService.findCanceledLoan();
	}

	/**
	 * Find loan status issued.
	 * 
	 * @author Ines Dridi
	 * @param idLoan the id loan
	 * @return the loan DTO
	 */
	@GetMapping("/loan-issued-by-id/{idLoanExtern}")
	public List<LoanDTO> findLoanStatusIssued(@PathVariable("idLoanExtern") Long idLoan) {

		return loanAbacusService.findIssuedLoans(Arrays.asList(idLoan));
	}

	/**
	 * Find active account for CUSTOMER and LOAN.
	 *
	 * @author HaythemBenizid
	 * @param idAccounts the id accounts
	 * @return the long
	 */
	@PostMapping("/find-active-account-by-customer-loan")
	public Long findActiveAccountForCustomerAndLoan(@RequestBody List<Long> idAccounts) {

		return loanAbacusService.findActiveAccountForCustomerAndLoan(idAccounts);
	}

	/**
	 * Find loans by given params used by button synchronize.
	 * 
	 * @author idridi
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/loans-in-abacus/{limite}")
	public List<LoanDTO> findLoans(@PathVariable("limite") Long limite) {

		return loanAbacusService.find(limite);
	}

	/**
	 * Gets the closing balanceby id loan.
	 * 
	 * @author idridi
	 * @param idLoanExtern the id loan extern
	 * @return the closing balanceby id loan
	 */
	@GetMapping("/get-closing-balance-by-idLoan/{idLoanExtern}")
	public Long getClosingBalancebyIdLoan(@PathVariable("idLoanExtern") Long idLoanExtern) {

		return loanAbacusService.getClosingBalancebyIdLoan(idLoanExtern);
	}

	/**
	 * Check setting topup validity.
	 *
	 * @param loanDTO the loan DTO
	 * @return the setting topup validity DTO
	 */
	@PostMapping("/setting-topup/check-validity")
	public SettingTopupValidityDTO checkSettingTopupValidity(@RequestBody LoanDTO loanDTO) {

		return loanAbacusService.checkSettingTopupValidity(loanDTO);
	}
}
