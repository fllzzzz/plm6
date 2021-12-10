<template>
  <el-dialog
    title="开票审核"
    :visible="dlgVisible"
    :before-close="handleClose"
    wrapper-closable
    width="500px"
    top="10vh"
    append-to-body
  >
    <el-form ref="form" :model="form" :rules="rules" size="small" label-width="150px">
      <el-form-item label="收票单位">
        {{ form.invoiceCollector }}
      </el-form-item>
      <el-form-item label="开户行">
        {{ form.collectionDepositBank }}
      </el-form-item>
      <el-form-item label="银行账号">
        {{ form.collectionBankAccount }}
      </el-form-item>
      <el-form-item label="地址">
        {{ form.collectionAddress }}
      </el-form-item>
      <el-form-item label="电话">
        {{ form.collectionTel }}
      </el-form-item>
      <el-form-item label="发票类型">
        {{ form.invoiceTypeAndTaxRate }}
      </el-form-item>
      <el-form-item label="票面金额(元)">
        {{ form.invoiceAmount | toFixed($DP.YUAN) | toThousandFilter }}
      </el-form-item>
      <el-form-item label="税额">
        {{ form.tax }}
      </el-form-item>
      <el-form-item label="开票日期" prop="invoiceDate">
        <el-date-picker
          v-model="form.invoiceDate"
          type="date"
          :clearable="false"
          value-format="timestamp"
          placeholder="选择开票日期"
          style="width: 250px;"
          class="input-underline"
          @blur="change($event)"
        />
      </el-form-item>
      <el-form-item label="发票编号" prop="invoiceNo">
        <el-input
          v-model.trim="form.invoiceNo"
          type="text"
          placeholder="请填写发票编号"
          class="input-underline"
          style="width: 250px;"
          @input="change($event)"
        />
      </el-form-item>
      <el-form-item label="开票人" prop="invoiceUserId">
        <user-dept-cascader
          :value.sync="form.invoiceUserId"
          filterable
          clearable
          show-all-levels
          placeholder="请选择开票人"
          style="width:250px"
          class="input-underline"
        />
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model.trim="form.remark"
          type="text"
          placeholder="填写货物或应税劳务、服务名称"
          class="input-underline"
          style="width: 250px;"
          @input="change($event)"
        />
      </el-form-item>
    </el-form>
    <span slot="footer" class="dialog-footer">
      <el-button @click="handleClose">取 消</el-button>
      <el-button type="success" :disabled="!$isNotBlank(form.invoiceDate)" @click="subimt">通 过</el-button>
    </span>
  </el-dialog>
</template>

<script>
import { handleInvoicing } from '@/api/contract/collection/invoice'
import userDeptCascader from '@/views/components/base/user-dept-cascader'
import { judgeSameValue } from '@/utils'
import { validatorInvoiceNo } from '@/utils/validatePattern'

export default {
  components: { userDeptCascader },
  props: {
    visible: {
      type: Boolean,
      required: true
    },
    record: {
      type: Object,
      required: true
    }
  },
  data() {
    const validateInvoiceNo = (rule, value, callback) => {
      if (!validatorInvoiceNo.test(this.form.invoiceNo) && this.form.invoiceNo) {
        callback('请输入数字或字符')
      }
      if (this.form.invoiceNo && this.form.invoiceNo.length > 20 && this.form.invoiceNo) {
        callback('长度不可超过20字')
      }
      callback()
    }
    // const validateInvoiceUserId = (rule, value, callback) => {
    //   if (!this.form.invoiceUserId) {
    //     callback('请选择开票人')
    //   }
    //   callback()
    // }
    return {
      form: {
        invoiceDate: Date.now(), // 开票日期
        invoiceNo: undefined, // 发票编号
        invoiceUserId: undefined // 发票编号
      },
      rules: {
        invoiceNo: [
          { validator: validateInvoiceNo, trigger: 'blur' }
        ],
        invoiceDate: [
          { required: true, message: '请选择开票日期', trigger: 'change' }
        ]
        // invoiceUserId: [
        //   { validator: validateInvoiceUserId, trigger: 'change' }
        // ]
      }
    }
  },
  computed: {
    dlgVisible() {
      return this.visible
    }
  },
  watch: {
    record: {
      handler(newV, oldV) {
        if (!judgeSameValue(newV, oldV)) {
          this.init(newV)
        }
      },
      deep: true,
      immediate: true
    }
  },
  methods: {
    change(e) {
      this.$forceUpdate()
    },
    init(record) {
      this.form = {}
      this.form.invoiceDate = Date.now()
      this.form.invoiceNo = undefined
      this.form.invoiceUserId = undefined
      Object.assign(this.form, { ...record })
      this.form.invoiceNo += '' // 避免是数字类型的情况（el-input 数字类型校验会报错）
    },
    handleClose() {
      this.$emit('update:visible', false)
    },
    subimt() {
      this.$refs['form'].validate(async(valid) => {
        if (valid) {
          await handleInvoicing(this.form)
          this.$notify({ title: '审核已通过', type: 'success', duration: 2500 })
          this.$emit('success', this.form)
          this.handleClose()
        } else {
          return false
        }
      })
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
>>>.el-input-number .el-input__inner {
  text-align: left;
}
</style>
