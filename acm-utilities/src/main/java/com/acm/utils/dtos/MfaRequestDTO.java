package com.acm.utils.dtos;

/**
 * {@link MfaRequestDTO} is a Data Transfer Object (DTO) class representing a
 * request for
 * Multi-Factor Authentication (MFA). This class contains information such as
 * the national ID and
 * the service for which MFA is requested.
 */
public class MfaRequestDTO {
	private String nationalId;
	private String service;

	/**
	 * Get the national ID associated with the MFA request.
	 *
	 * @return The national ID as a string.
	 */
	public String getNationalId() {

		return nationalId;
	}

	/**
	 * Set the national ID for the MFA request.
	 *
	 * @param nationalId The national ID to set as a string.
	 */
	public void setNationalId(String nationalId) {

		this.nationalId = nationalId;
	}

	/**
	 * Get the service for which MFA is requested.
	 *
	 * @return The service name as a string.
	 */
	public String getService() {

		return service;
	}

	/**
	 * Set the service for which MFA is requested.
	 *
	 * @param service The service name to set as a string.
	 */
	public void setService(String service) {

		this.service = service;
	}

	/**
	 * Returns a string representation of the MfaRequestDTO object.
	 *
	 * @return A string representation containing the national ID and service.
	 */
	@Override
	public String toString() {
		return "MfaRequestDTO{" +
				"nationalId='" + nationalId + '\'' +
				", service='" + service + '\'' +
				'}';
	}

}
