/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * the {@link FinancialAnalysisDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class FinancialAnalysisDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1771847372444273681L;

	/** The current assets. */
	private BigDecimal currentAssets;

	/** The fixed assets. */
	private BigDecimal fixedAssets;

	/** The assets. */
	private BigDecimal assets;

	/** The current liabilities. */
	private BigDecimal currentLiabilities;

	/** The fixed liabilities. */
	private BigDecimal fixedLiabilities;

	/** The total liabilities. */
	private BigDecimal totalLiabilities;

	/** The equity. */
	private BigDecimal equity;

	/** The monthly sales. */
	private BigDecimal monthlySales;

	/** The cots of good sold. */
	private BigDecimal cotsOfGoodSold;

	/** The gross profit. */
	private BigDecimal grossProfit;

	/** The monthly expense. */
	private BigDecimal monthlyExpense;

	/** The net profit. */
	private BigDecimal netProfit;

	/** The other income. */
	private BigDecimal otherIncome;

	/** The family expense. */
	private BigDecimal familyExpense;

	/** The household surplus. */
	private BigDecimal householdSurplus;

	/** The excluded assets. */
	private BigDecimal excludedAssets;

	/** The collateral amount. */
	private BigDecimal collateralAmount;

	/** The apply amount total. */
	private BigDecimal applyAmountTotal;

	/** The maturity date. */
	private Date maturityDate;

	/** The loan term. */
	private BigDecimal loanTerm;

	/** The installement. */
	private BigDecimal installement;

	/** The working capital. */
	private BigDecimal workingCapital;

	/** The gross margin. */
	private BigDecimal grossMargin;

	/** The debt equity. */
	private BigDecimal debtEquity;

	/** The inventory turnover. */
	private BigDecimal inventoryTurnover;

	/** The installment coverage. */
	private BigDecimal installmentCoverage;

	/** The loan recommendation. */
	private BigDecimal loanRecommendation;

	/** The guarantor coverage. */
	private BigDecimal guarantorCoverage;

	/**
	 * Instantiates a new financial analysis DTO.
	 */
	public FinancialAnalysisDTO() {

		/*
		 * Empty
		 */
	}

	/**
	 * Instantiates a new financial analysis DTO.
	 *
	 * @param currentAssets the current assets
	 * @param fixedAssets the fixed assets
	 * @param assets the assets
	 * @param currentLiabilities the current liabilities
	 * @param fixedLiabilities the fixed liabilities
	 * @param totalLiabilities the total liabilities
	 * @param equity the equity
	 * @param monthlySales the monthly sales
	 * @param cotsOfGoodSold the cots of good sold
	 * @param grossProfit the gross profit
	 * @param monthlyExpense the monthly expense
	 * @param netProfit the net profit
	 * @param otherIncome the other income
	 * @param familyExpense the family expense
	 * @param householdSurplus the household surplus
	 * @param excludedAssets the excluded assets
	 * @param collateralAmount the collateral amount
	 * @param applyAmountTotal the apply amount total
	 * @param maturityDate the maturity date
	 * @param loanTerm the loan term
	 * @param installement the installement
	 * @param workingCapital the working capital
	 * @param grossMargin the gross margin
	 * @param debtEquity the debt equity
	 * @param inventoryTurnover the inventory turnover
	 * @param installmentCoverage the installment coverage
	 * @param loanRecommendation the loan recommendation
	 * @param guarantorCoverage the guarantor coverage
	 */
	public FinancialAnalysisDTO(BigDecimal currentAssets, BigDecimal fixedAssets, BigDecimal assets,
			BigDecimal currentLiabilities, BigDecimal fixedLiabilities, BigDecimal totalLiabilities,
			BigDecimal equity, BigDecimal monthlySales, BigDecimal cotsOfGoodSold,
			BigDecimal grossProfit, BigDecimal monthlyExpense, BigDecimal netProfit,
			BigDecimal otherIncome, BigDecimal familyExpense, BigDecimal householdSurplus,
			BigDecimal excludedAssets, BigDecimal collateralAmount, BigDecimal applyAmountTotal,
			Date maturityDate, BigDecimal loanTerm, BigDecimal installement,
			BigDecimal workingCapital, BigDecimal grossMargin, BigDecimal debtEquity,
			BigDecimal inventoryTurnover, BigDecimal installmentCoverage,
			BigDecimal loanRecommendation, BigDecimal guarantorCoverage) {

		this.currentAssets = currentAssets;
		this.fixedAssets = fixedAssets;
		this.assets = assets;
		this.currentLiabilities = currentLiabilities;
		this.fixedLiabilities = fixedLiabilities;
		this.totalLiabilities = totalLiabilities;
		this.equity = equity;
		this.monthlySales = monthlySales;
		this.cotsOfGoodSold = cotsOfGoodSold;
		this.grossProfit = grossProfit;
		this.monthlyExpense = monthlyExpense;
		this.netProfit = netProfit;
		this.otherIncome = otherIncome;
		this.familyExpense = familyExpense;
		this.householdSurplus = householdSurplus;
		this.excludedAssets = excludedAssets;
		this.collateralAmount = collateralAmount;
		this.applyAmountTotal = applyAmountTotal;
		this.maturityDate = maturityDate;
		this.loanTerm = loanTerm;
		this.installement = installement;
		this.workingCapital = workingCapital;
		this.grossMargin = grossMargin;
		this.debtEquity = debtEquity;
		this.inventoryTurnover = inventoryTurnover;
		this.installmentCoverage = installmentCoverage;
		this.loanRecommendation = loanRecommendation;
		this.guarantorCoverage = guarantorCoverage;
	}

	/**
	 * Gets the current assets.
	 *
	 * @return the currentAssets
	 */
	public BigDecimal getCurrentAssets() {

		return currentAssets;
	}

	/**
	 * Sets the current assets.
	 *
	 * @param currentAssets the currentAssets to set
	 */
	public void setCurrentAssets(BigDecimal currentAssets) {

		this.currentAssets = currentAssets;
	}

	/**
	 * Gets the fixed assets.
	 *
	 * @return the fixedAssets
	 */
	public BigDecimal getFixedAssets() {

		return fixedAssets;
	}

	/**
	 * Sets the fixed assets.
	 *
	 * @param fixedAssets the fixedAssets to set
	 */
	public void setFixedAssets(BigDecimal fixedAssets) {

		this.fixedAssets = fixedAssets;
	}

	/**
	 * Gets the assets.
	 *
	 * @return the assets
	 */
	public BigDecimal getAssets() {

		return assets;
	}

	/**
	 * Sets the assets.
	 *
	 * @param assets the assets to set
	 */
	public void setAssets(BigDecimal assets) {

		this.assets = assets;
	}

	/**
	 * Gets the current liabilities.
	 *
	 * @return the currentLiabilities
	 */
	public BigDecimal getCurrentLiabilities() {

		return currentLiabilities;
	}

	/**
	 * Sets the current liabilities.
	 *
	 * @param currentLiabilities the currentLiabilities to set
	 */
	public void setCurrentLiabilities(BigDecimal currentLiabilities) {

		this.currentLiabilities = currentLiabilities;
	}

	/**
	 * Gets the fixed liabilities.
	 *
	 * @return the fixedLiabilities
	 */
	public BigDecimal getFixedLiabilities() {

		return fixedLiabilities;
	}

	/**
	 * Sets the fixed liabilities.
	 *
	 * @param fixedLiabilities the fixedLiabilities to set
	 */
	public void setFixedLiabilities(BigDecimal fixedLiabilities) {

		this.fixedLiabilities = fixedLiabilities;
	}

	/**
	 * Gets the total liabilities.
	 *
	 * @return the totalLiabilities
	 */
	public BigDecimal getTotalLiabilities() {

		return totalLiabilities;
	}

	/**
	 * Sets the total liabilities.
	 *
	 * @param totalLiabilities the totalLiabilities to set
	 */
	public void setTotalLiabilities(BigDecimal totalLiabilities) {

		this.totalLiabilities = totalLiabilities;
	}

	/**
	 * Gets the equity.
	 *
	 * @return the equity
	 */
	public BigDecimal getEquity() {

		return equity;
	}

	/**
	 * Sets the equity.
	 *
	 * @param equity the equity to set
	 */
	public void setEquity(BigDecimal equity) {

		this.equity = equity;
	}

	/**
	 * Gets the monthly sales.
	 *
	 * @return the monthlySales
	 */
	public BigDecimal getMonthlySales() {

		return monthlySales;
	}

	/**
	 * Sets the monthly sales.
	 *
	 * @param monthlySales the monthlySales to set
	 */
	public void setMonthlySales(BigDecimal monthlySales) {

		this.monthlySales = monthlySales;
	}

	/**
	 * Gets the cots of good sold.
	 *
	 * @return the cotsOfGoodSold
	 */
	public BigDecimal getCotsOfGoodSold() {

		return cotsOfGoodSold;
	}

	/**
	 * Sets the cots of good sold.
	 *
	 * @param cotsOfGoodSold the cotsOfGoodSold to set
	 */
	public void setCotsOfGoodSold(BigDecimal cotsOfGoodSold) {

		this.cotsOfGoodSold = cotsOfGoodSold;
	}

	/**
	 * Gets the gross profit.
	 *
	 * @return the grossProfit
	 */
	public BigDecimal getGrossProfit() {

		return grossProfit;
	}

	/**
	 * Sets the gross profit.
	 *
	 * @param grossProfit the grossProfit to set
	 */
	public void setGrossProfit(BigDecimal grossProfit) {

		this.grossProfit = grossProfit;
	}

	/**
	 * Gets the monthly expense.
	 *
	 * @return the monthlyExpense
	 */
	public BigDecimal getMonthlyExpense() {

		return monthlyExpense;
	}

	/**
	 * Sets the monthly expense.
	 *
	 * @param monthlyExpense the monthlyExpense to set
	 */
	public void setMonthlyExpense(BigDecimal monthlyExpense) {

		this.monthlyExpense = monthlyExpense;
	}

	/**
	 * Gets the net profit.
	 *
	 * @return the netProfit
	 */
	public BigDecimal getNetProfit() {

		return netProfit;
	}

	/**
	 * Sets the net profit.
	 *
	 * @param netProfit the netProfit to set
	 */
	public void setNetProfit(BigDecimal netProfit) {

		this.netProfit = netProfit;
	}

	/**
	 * Gets the other income.
	 *
	 * @return the otherIncome
	 */
	public BigDecimal getOtherIncome() {

		return otherIncome;
	}

	/**
	 * Sets the other income.
	 *
	 * @param otherIncome the otherIncome to set
	 */
	public void setOtherIncome(BigDecimal otherIncome) {

		this.otherIncome = otherIncome;
	}

	/**
	 * Gets the family expense.
	 *
	 * @return the familyExpense
	 */
	public BigDecimal getFamilyExpense() {

		return familyExpense;
	}

	/**
	 * Sets the family expense.
	 *
	 * @param familyExpense the familyExpense to set
	 */
	public void setFamilyExpense(BigDecimal familyExpense) {

		this.familyExpense = familyExpense;
	}

	/**
	 * Gets the household surplus.
	 *
	 * @return the householdSurplus
	 */
	public BigDecimal getHouseholdSurplus() {

		return householdSurplus;
	}

	/**
	 * Sets the household surplus.
	 *
	 * @param householdSurplus the householdSurplus to set
	 */
	public void setHouseholdSurplus(BigDecimal householdSurplus) {

		this.householdSurplus = householdSurplus;
	}

	/**
	 * Gets the excluded assets.
	 *
	 * @return the excludedAssets
	 */
	public BigDecimal getExcludedAssets() {

		return excludedAssets;
	}

	/**
	 * Sets the excluded assets.
	 *
	 * @param excludedAssets the excludedAssets to set
	 */
	public void setExcludedAssets(BigDecimal excludedAssets) {

		this.excludedAssets = excludedAssets;
	}

	/**
	 * Gets the collateral amount.
	 *
	 * @return the collateralAmount
	 */
	public BigDecimal getCollateralAmount() {

		return collateralAmount;
	}

	/**
	 * Sets the collateral amount.
	 *
	 * @param collateralAmount the collateralAmount to set
	 */
	public void setCollateralAmount(BigDecimal collateralAmount) {

		this.collateralAmount = collateralAmount;
	}

	/**
	 * Gets the apply amount total.
	 *
	 * @return the applyAmountTotal
	 */
	public BigDecimal getApplyAmountTotal() {

		return applyAmountTotal;
	}

	/**
	 * Sets the apply amount total.
	 *
	 * @param applyAmountTotal the applyAmountTotal to set
	 */
	public void setApplyAmountTotal(BigDecimal applyAmountTotal) {

		this.applyAmountTotal = applyAmountTotal;
	}

	/**
	 * Gets the maturity date.
	 *
	 * @return the maturityDate
	 */
	public Date getMaturityDate() {

		return maturityDate;
	}

	/**
	 * Sets the maturity date.
	 *
	 * @param maturityDate the maturityDate to set
	 */
	public void setMaturityDate(Date maturityDate) {

		this.maturityDate = maturityDate;
	}

	/**
	 * Gets the loan term.
	 *
	 * @return the loanTerm
	 */
	public BigDecimal getLoanTerm() {

		return loanTerm;
	}

	/**
	 * Sets the loan term.
	 *
	 * @param loanTerm the loanTerm to set
	 */
	public void setLoanTerm(BigDecimal loanTerm) {

		this.loanTerm = loanTerm;
	}

	/**
	 * Gets the installement.
	 *
	 * @return the installement
	 */
	public BigDecimal getInstallement() {

		return installement;
	}

	/**
	 * Sets the installement.
	 *
	 * @param installement the installement to set
	 */
	public void setInstallement(BigDecimal installement) {

		this.installement = installement;
	}

	/**
	 * Gets the working capital.
	 *
	 * @return the workingCapital
	 */
	public BigDecimal getWorkingCapital() {

		return workingCapital;
	}

	/**
	 * Sets the working capital.
	 *
	 * @param workingCapital the workingCapital to set
	 */
	public void setWorkingCapital(BigDecimal workingCapital) {

		this.workingCapital = workingCapital;
	}

	/**
	 * Gets the gross margin.
	 *
	 * @return the grossMargin
	 */
	public BigDecimal getGrossMargin() {

		return grossMargin;
	}

	/**
	 * Sets the gross margin.
	 *
	 * @param grossMargin the grossMargin to set
	 */
	public void setGrossMargin(BigDecimal grossMargin) {

		this.grossMargin = grossMargin;
	}

	/**
	 * Gets the debt equity.
	 *
	 * @return the debtEquity
	 */
	public BigDecimal getDebtEquity() {

		return debtEquity;
	}

	/**
	 * Sets the debt equity.
	 *
	 * @param debtEquity the debtEquity to set
	 */
	public void setDebtEquity(BigDecimal debtEquity) {

		this.debtEquity = debtEquity;
	}

	/**
	 * Gets the inventory turnover.
	 *
	 * @return the inventoryTurnover
	 */
	public BigDecimal getInventoryTurnover() {

		return inventoryTurnover;
	}

	/**
	 * Sets the inventory turnover.
	 *
	 * @param inventoryTurnover the inventoryTurnover to set
	 */
	public void setInventoryTurnover(BigDecimal inventoryTurnover) {

		this.inventoryTurnover = inventoryTurnover;
	}

	/**
	 * Gets the installment coverage.
	 *
	 * @return the installmentCoverage
	 */
	public BigDecimal getInstallmentCoverage() {

		return installmentCoverage;
	}

	/**
	 * Sets the installment coverage.
	 *
	 * @param installmentCoverage the installmentCoverage to set
	 */
	public void setInstallmentCoverage(BigDecimal installmentCoverage) {

		this.installmentCoverage = installmentCoverage;
	}

	/**
	 * Gets the loan recommendation.
	 *
	 * @return the loanRecommendation
	 */
	public BigDecimal getLoanRecommendation() {

		return loanRecommendation;
	}

	/**
	 * Sets the loan recommendation.
	 *
	 * @param loanRecommendation the loanRecommendation to set
	 */
	public void setLoanRecommendation(BigDecimal loanRecommendation) {

		this.loanRecommendation = loanRecommendation;
	}

	/**
	 * Gets the guarantor coverage.
	 *
	 * @return the guarantorCoverage
	 */
	public BigDecimal getGuarantorCoverage() {

		return guarantorCoverage;
	}

	/**
	 * Sets the guarantor coverage.
	 *
	 * @param guarantorCoverage the guarantorCoverage to set
	 */
	public void setGuarantorCoverage(BigDecimal guarantorCoverage) {

		this.guarantorCoverage = guarantorCoverage;
	}

}
