/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.List;

/**
 * {@link CustomerAbacusAPIModelPerson} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelPerson implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -9038530110968674252L;

	/** The person ID. */
	private int personID;

	/** The title. */
	private int title;

	/** The gender. */
	private int gender;

	/** The forename part one. */
	private String forenamePartOne;

	/** The forename part two. */
	private String forenamePartTwo;

	/** The forename part three. */
	private String forenamePartThree;

	/** The surname. */
	private String surname;

	/** The social security number. */
	private String socialSecurityNumber;

	/** The driving licence number. */
	private String drivingLicenceNumber;

	/** The date of birth. */
	private String dateOfBirth;

	/** The telephone 1. */
	private String telephone1;

	/** The telephone 2. */
	private String telephone2;

	/** The telephone 3. */
	private String telephone3;

	/** The sms pin. */
	private String smsPin;

	/** The e mail. */
	private String eMail;

	/** The finger prints. */
	private List<Object> fingerPrints;

	/** The picture ID. */
	private int pictureID;

	/** The card picture ID. */
	private int cardPictureID;

	/** The signature ID. */
	private int signatureID;

	/** The employers. */
	private List<Object> employers;

	/** The monthly pension. */
	private double monthlyPension;

	/** The approved benefits. */
	private double approvedBenefits;

	/** The other income. */
	private double otherIncome;

	/** The monthly pension partner. */
	private double monthlyPensionPartner;

	/** The approved benefits partner. */
	private double approvedBenefitsPartner;

	/** The other income partner. */
	private double otherIncomePartner;

	/** The temp token. */
	private String tempToken;

	/** The delete fingerprints. */
	private String deleteFingerprints;

	/** The sms new pin. */
	private String smsNewPin;

	/** The override reason id. */
	private String overrideReasonId;

	/**
	 * Gets the person ID.
	 *
	 * @return the personID
	 */
	public int getPersonID() {

		return personID;
	}

	/**
	 * Sets the person ID.
	 *
	 * @param personID the personID to set
	 */
	public void setPersonID(int personID) {

		this.personID = personID;
	}

	/**
	 * Gets the title.
	 *
	 * @return the title
	 */
	public int getTitle() {

		return title;
	}

	/**
	 * Sets the title.
	 *
	 * @param title the title to set
	 */
	public void setTitle(int title) {

		this.title = title;
	}

	/**
	 * Gets the gender.
	 *
	 * @return the gender
	 */
	public int getGender() {

		return gender;
	}

	/**
	 * Sets the gender.
	 *
	 * @param gender the gender to set
	 */
	public void setGender(int gender) {

		this.gender = gender;
	}

	/**
	 * Gets the forename part one.
	 *
	 * @return the forenamePartOne
	 */
	public String getForenamePartOne() {

		return forenamePartOne;
	}

	/**
	 * Sets the forename part one.
	 *
	 * @param forenamePartOne the forenamePartOne to set
	 */
	public void setForenamePartOne(String forenamePartOne) {

		this.forenamePartOne = forenamePartOne;
	}

	/**
	 * Gets the forename part two.
	 *
	 * @return the forenamePartTwo
	 */
	public String getForenamePartTwo() {

		return forenamePartTwo;
	}

	/**
	 * Sets the forename part two.
	 *
	 * @param forenamePartTwo the forenamePartTwo to set
	 */
	public void setForenamePartTwo(String forenamePartTwo) {

		this.forenamePartTwo = forenamePartTwo;
	}

	/**
	 * Gets the forename part three.
	 *
	 * @return the forenamePartThree
	 */
	public String getForenamePartThree() {

		return forenamePartThree;
	}

	/**
	 * Sets the forename part three.
	 *
	 * @param forenamePartThree the forenamePartThree to set
	 */
	public void setForenamePartThree(String forenamePartThree) {

		this.forenamePartThree = forenamePartThree;
	}

	/**
	 * Gets the surname.
	 *
	 * @return the surname
	 */
	public String getSurname() {

		return surname;
	}

	/**
	 * Sets the surname.
	 *
	 * @param surname the surname to set
	 */
	public void setSurname(String surname) {

		this.surname = surname;
	}

	/**
	 * Gets the social security number.
	 *
	 * @return the socialSecurityNumber
	 */
	public String getSocialSecurityNumber() {

		return socialSecurityNumber;
	}

	/**
	 * Sets the social security number.
	 *
	 * @param socialSecurityNumber the socialSecurityNumber to set
	 */
	public void setSocialSecurityNumber(String socialSecurityNumber) {

		this.socialSecurityNumber = socialSecurityNumber;
	}

	/**
	 * Gets the driving licence number.
	 *
	 * @return the drivingLicenceNumber
	 */
	public String getDrivingLicenceNumber() {

		return drivingLicenceNumber;
	}

	/**
	 * Sets the driving licence number.
	 *
	 * @param drivingLicenceNumber the drivingLicenceNumber to set
	 */
	public void setDrivingLicenceNumber(String drivingLicenceNumber) {

		this.drivingLicenceNumber = drivingLicenceNumber;
	}

	/**
	 * Gets the date of birth.
	 *
	 * @return the dateOfBirth
	 */
	public String getDateOfBirth() {

		return dateOfBirth;
	}

	/**
	 * Sets the date of birth.
	 *
	 * @param dateOfBirth the dateOfBirth to set
	 */
	public void setDateOfBirth(String dateOfBirth) {

		this.dateOfBirth = dateOfBirth;
	}

	/**
	 * Gets the telephone 1.
	 *
	 * @return the telephone1
	 */
	public String getTelephone1() {

		return telephone1;
	}

	/**
	 * Sets the telephone 1.
	 *
	 * @param telephone1 the telephone1 to set
	 */
	public void setTelephone1(String telephone1) {

		this.telephone1 = telephone1;
	}

	/**
	 * Gets the telephone 2.
	 *
	 * @return the telephone2
	 */
	public String getTelephone2() {

		return telephone2;
	}

	/**
	 * Sets the telephone 2.
	 *
	 * @param telephone2 the telephone2 to set
	 */
	public void setTelephone2(String telephone2) {

		this.telephone2 = telephone2;
	}

	/**
	 * Gets the telephone 3.
	 *
	 * @return the telephone3
	 */
	public String getTelephone3() {

		return telephone3;
	}

	/**
	 * Sets the telephone 3.
	 *
	 * @param telephone3 the telephone3 to set
	 */
	public void setTelephone3(String telephone3) {

		this.telephone3 = telephone3;
	}

	/**
	 * Gets the sms pin.
	 *
	 * @return the smsPin
	 */
	public String getSmsPin() {

		return smsPin;
	}

	/**
	 * Sets the sms pin.
	 *
	 * @param smsPin the smsPin to set
	 */
	public void setSmsPin(String smsPin) {

		this.smsPin = smsPin;
	}

	/**
	 * Gets the e mail.
	 *
	 * @return the eMail
	 */
	public String geteMail() {

		return eMail;
	}

	/**
	 * Sets the e mail.
	 *
	 * @param eMail the eMail to set
	 */
	public void seteMail(String eMail) {

		this.eMail = eMail;
	}

	/**
	 * Gets the finger prints.
	 *
	 * @return the fingerPrints
	 */
	public List<Object> getFingerPrints() {

		return fingerPrints;
	}

	/**
	 * Sets the finger prints.
	 *
	 * @param fingerPrints the fingerPrints to set
	 */
	public void setFingerPrints(List<Object> fingerPrints) {

		this.fingerPrints = fingerPrints;
	}

	/**
	 * Gets the picture ID.
	 *
	 * @return the pictureID
	 */
	public int getPictureID() {

		return pictureID;
	}

	/**
	 * Sets the picture ID.
	 *
	 * @param pictureID the pictureID to set
	 */
	public void setPictureID(int pictureID) {

		this.pictureID = pictureID;
	}

	/**
	 * Gets the card picture ID.
	 *
	 * @return the cardPictureID
	 */
	public int getCardPictureID() {

		return cardPictureID;
	}

	/**
	 * Sets the card picture ID.
	 *
	 * @param cardPictureID the cardPictureID to set
	 */
	public void setCardPictureID(int cardPictureID) {

		this.cardPictureID = cardPictureID;
	}

	/**
	 * Gets the signature ID.
	 *
	 * @return the signatureID
	 */
	public int getSignatureID() {

		return signatureID;
	}

	/**
	 * Sets the signature ID.
	 *
	 * @param signatureID the signatureID to set
	 */
	public void setSignatureID(int signatureID) {

		this.signatureID = signatureID;
	}

	/**
	 * Gets the employers.
	 *
	 * @return the employers
	 */
	public List<Object> getEmployers() {

		return employers;
	}

	/**
	 * Sets the employers.
	 *
	 * @param employers the employers to set
	 */
	public void setEmployers(List<Object> employers) {

		this.employers = employers;
	}

	/**
	 * Gets the override reason id.
	 *
	 * @return the overrideReasonId
	 */
	public String getOverrideReasonId() {

		return overrideReasonId;
	}

	/**
	 * Sets the override reason id.
	 *
	 * @param overrideReasonId the overrideReasonId to set
	 */
	public void setOverrideReasonId(String overrideReasonId) {

		this.overrideReasonId = overrideReasonId;
	}

	/**
	 * Gets the monthly pension.
	 *
	 * @return the monthlyPension
	 */
	public double getMonthlyPension() {

		return monthlyPension;
	}

	/**
	 * Sets the monthly pension.
	 *
	 * @param monthlyPension the monthlyPension to set
	 */
	public void setMonthlyPension(double monthlyPension) {

		this.monthlyPension = monthlyPension;
	}

	/**
	 * Gets the approved benefits.
	 *
	 * @return the approvedBenefits
	 */
	public double getApprovedBenefits() {

		return approvedBenefits;
	}

	/**
	 * Sets the approved benefits.
	 *
	 * @param approvedBenefits the approvedBenefits to set
	 */
	public void setApprovedBenefits(double approvedBenefits) {

		this.approvedBenefits = approvedBenefits;
	}

	/**
	 * Gets the other income.
	 *
	 * @return the otherIncome
	 */
	public double getOtherIncome() {

		return otherIncome;
	}

	/**
	 * Sets the other income.
	 *
	 * @param otherIncome the otherIncome to set
	 */
	public void setOtherIncome(double otherIncome) {

		this.otherIncome = otherIncome;
	}

	/**
	 * Gets the monthly pension partner.
	 *
	 * @return the monthlyPensionPartner
	 */
	public double getMonthlyPensionPartner() {

		return monthlyPensionPartner;
	}

	/**
	 * Sets the monthly pension partner.
	 *
	 * @param monthlyPensionPartner the monthlyPensionPartner to set
	 */
	public void setMonthlyPensionPartner(double monthlyPensionPartner) {

		this.monthlyPensionPartner = monthlyPensionPartner;
	}

	/**
	 * Gets the approved benefits partner.
	 *
	 * @return the approvedBenefitsPartner
	 */
	public double getApprovedBenefitsPartner() {

		return approvedBenefitsPartner;
	}

	/**
	 * Sets the approved benefits partner.
	 *
	 * @param approvedBenefitsPartner the approvedBenefitsPartner to set
	 */
	public void setApprovedBenefitsPartner(double approvedBenefitsPartner) {

		this.approvedBenefitsPartner = approvedBenefitsPartner;
	}

	/**
	 * Gets the other income partner.
	 *
	 * @return the otherIncomePartner
	 */
	public double getOtherIncomePartner() {

		return otherIncomePartner;
	}

	/**
	 * Sets the other income partner.
	 *
	 * @param otherIncomePartner the otherIncomePartner to set
	 */
	public void setOtherIncomePartner(double otherIncomePartner) {

		this.otherIncomePartner = otherIncomePartner;
	}

	/**
	 * Gets the temp token.
	 *
	 * @return the tempToken
	 */
	public String getTempToken() {

		return tempToken;
	}

	/**
	 * Sets the temp token.
	 *
	 * @param tempToken the tempToken to set
	 */
	public void setTempToken(String tempToken) {

		this.tempToken = tempToken;
	}

	/**
	 * Gets the delete fingerprints.
	 *
	 * @return the deleteFingerprints
	 */
	public String getDeleteFingerprints() {

		return deleteFingerprints;
	}

	/**
	 * Sets the delete fingerprints.
	 *
	 * @param deleteFingerprints the deleteFingerprints to set
	 */
	public void setDeleteFingerprints(String deleteFingerprints) {

		this.deleteFingerprints = deleteFingerprints;
	}

	/**
	 * Gets the sms new pin.
	 *
	 * @return the smsNewPin
	 */
	public String getSmsNewPin() {

		return smsNewPin;
	}

	/**
	 * Sets the sms new pin.
	 *
	 * @param smsNewPin the smsNewPin to set
	 */
	public void setSmsNewPin(String smsNewPin) {

		this.smsNewPin = smsNewPin;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelPerson [personID=" + personID + ", title=" + title
				+ ", gender=" + gender + ", forenamePartOne=" + forenamePartOne
				+ ", forenamePartTwo=" + forenamePartTwo + ", forenamePartThree="
				+ forenamePartThree + ", surname=" + surname + ", socialSecurityNumber="
				+ socialSecurityNumber + ", drivingLicenceNumber=" + drivingLicenceNumber
				+ ", dateOfBirth=" + dateOfBirth + ", telephone1=" + telephone1 + ", telephone2="
				+ telephone2 + ", telephone3=" + telephone3 + ", smsPin=" + smsPin + ", eMail="
				+ eMail + ", fingerPrints=" + fingerPrints + ", pictureID=" + pictureID
				+ ", cardPictureID=" + cardPictureID + ", signatureID=" + signatureID
				+ ", employers=" + employers + ", monthlyPension=" + monthlyPension
				+ ", approvedBenefits=" + approvedBenefits + ", otherIncome=" + otherIncome
				+ ", monthlyPensionPartner=" + monthlyPensionPartner + ", approvedBenefitsPartner="
				+ approvedBenefitsPartner + ", otherIncomePartner=" + otherIncomePartner
				+ ", tempToken=" + tempToken + ", deleteFingerprints=" + deleteFingerprints
				+ ", smsNewPin=" + smsNewPin + "]";
	}

}
