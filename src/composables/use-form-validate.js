
import { watch, nextTick } from 'vue'

/**
 * 用于表单自带校验未及时触发的情况
 * @param {component} formRef formRef
 * @param {object} form 表单对象
 * @param {array} fields 需要校验的字段
 */
export default function useFormValidate(formRef, form, fields) {
  nextTick(() => {
    const fls = fields || Object.keys(form)
    fls.forEach(field => {
      watch(
        () => form[field],
        () => { formRef.value.validateField(field) })
    })
  })
}
