export default {

  methods: {
    async $openReasonMsg({
      title = '请描述原因',
      tip,
      required = true
    }) {
      const inputValidator = (val) => {
        if ((!val || !val.trim()) && val !== 0) {
          return '必填'
        }
        if (val.length > 512) {
          return '长度在 1 到 512 个字符'
        }
        return true
      }
      try {
        const { value } = await this.$prompt(tip, title, {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          inputType: 'textarea',
          inputValidator: required ? inputValidator : null
        })
        // await setReasonMsg(data)
        return value
      } catch (error) {
        throw new Error(error)
      }
    }
  }
}
