package scunits

package object other extends system.other.Pressure with system.other.Acceleration
package object si extends system.si.Base with system.si.Prefix with system.si.Accepted
package object planck extends system.Planck
package object us extends system.us.InternationalFoot with system.us.Mass.Avoirdupois with system.us.Nautical with system.us.Base {
  object Survey extends system.us.SurveyFoot
  object Fluid extends system.us.volume.Fluid
  object Dry extends system.us.volume.Dry
  object Troy extends system.us.Mass.Troy
}
package object info extends system.info.Bits with system.info.Prefix