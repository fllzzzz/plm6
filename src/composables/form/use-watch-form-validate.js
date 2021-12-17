
import { watch, nextTick } from 'vue'

/**
 * 用于表单自带校验未及时触发的情况
 * @param {component} formRef formRef
 * @param {object} form 表单对象
 * @param {array} fields 需要校验的字段 数组中可包含数组['areaIds', ['basicClass', 'strucAreaIds', 'enclAreaIds']]
 */
export default function useWatchFormValidate(formRef, form, fields) {
  nextTick(() => {
    const fls = fields || Object.keys(form.value)
    console.log(formRef, form, fields)
    fls.forEach(field => {
      let _wField
      let _vField
      if (Array.isArray(field)) { // 可传入数组类型，避免监听字段与校验字段不一致的情况
        if (Array.isArray(field[1])) {
          _wField = []
          field[1].forEach(v => {
            _wField.push(() => form.value[v])
          })
        } else {
          _wField = () => form.value[_wField]
        }
        _vField = field[0] // 校验字段
      } else {
        _vField = field
        _wField = () => form.value[_vField]
      }
      watch(
        _wField,
        () => {
          formRef.value && formRef.value.validateField(_vField)
        })
    })
  })
}
