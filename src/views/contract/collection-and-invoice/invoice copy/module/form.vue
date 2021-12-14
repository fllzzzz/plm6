<template>
  <el-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    direction="rtl"
    size="50%"
  >
    <template slot="title">
      <span>开票填报</span>
      <el-popconfirm
        :title="`确认提交？`"
        placement="bottom-end"
        @confirm="crud.submitCU"
      >
        <el-button slot="reference" :loading="crud.status.cu === 2" type="success" size="mini">提交</el-button>
      </el-popconfirm>
    </template>
    <el-form ref="form" :model="form" :rules="rules" size="small" label-width="140px">
      <div class="form-row" style="display:flex;">
        <el-form-item label="项目" prop="projectId">
          <switch-project
            :value.sync="form.projectId"
            :initial="false"
            style="width:250px"
            class="filter-item"
            @change="handleProjectChange"
          />
        </el-form-item>
        <el-form-item label="发票类型" prop="invoiceType">
          <common-select
            :value.sync="form.invoiceType"
            :options="invoiceTypeEnum"
            type="enum"
            size="small"
            placeholder="请选择发票类型"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;" />
      <div class="form-row" style="display:flex;">
        <el-form-item label="合同金额(元)">
          <el-input
            v-model="project.contractAmount"
            type="text"
            placeholder="合同金额"
            style="width: 250px;"
            disabled
          />
        </el-form-item>
        <el-form-item label="销项税额" prop="managementFeeRate">
          <el-input
            v-model="project.contractAmount1"
            :readonly="true"
            style="width:110px"
            placeholder="先输入税率"
          />
          <e-input-number
            v-model="form.managementFeeRate"
            :step="1"
            :min="0"
            :max="100"
            :precision="$DP.ACCOUNTING"
            :controls="false"
            controls-position="right"
            style="width:80px"
            placeholder="0-100"
          />%
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="开票单位" prop="collectionCompany">
          <el-select v-model="form.collectionCompany" placeholder="请选择" style="width:250px;">
            <el-option
              v-for="item in companyList"
              :key="item.id"
              :label="item.name"
              :value="item.id"
            />
          </el-select>
        </el-form-item>
        <el-form-item label="发票日期" prop="paymentDate">
          <el-date-picker
            v-model="form.paymentDate"
            type="date"
            value-format="timestamp"
            placeholder="选择发票日期"
            style="width: 250px;"
          />
        </el-form-item>
        <!--        <el-form-item label="收款行" prop="collectionDepositBank">-->
        <!--          <el-input-->
        <!--            v-model="form.collectionDepositBank"-->
        <!--            type="text"-->
        <!--            placeholder="收款行"-->
        <!--            style="width: 250px;"-->
        <!--          />-->
        <!--          <bank-account-button-popover placement="bottom-end" :default="prepared === crud.status.add" :company-name="form.collectionCompany" @row-click="handleDepositBankChange" />-->
        <!--        </el-form-item>-->
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="收票单位" prop="collectionBankAccount1">
          <el-input
            v-model="form.collectionBankAccount1"
            type="text"
            placeholder="收票单位"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="发票号码" prop="collectionBankAccount1">
          <el-input
            v-model="form.collectionBankAccount1"
            type="text"
            placeholder="收票单位"
            style="width: 250px;"
          />
        </el-form-item>
      </div>
      <div class="form-row" style="display:flex;">
        <el-form-item label="发票面额(元)" prop="paymentAmount">
          <el-input-number
            v-model.number="form.paymentAmount"
            :min="-99999999999"
            :max="99999999999"
            :step="10000"
            :precision="$DP.YUAN"
            placeholder="发票面额"
            controls-position="right"
            style="width: 250px;"
          />
        </el-form-item>
        <el-form-item label="附件">
          <upload-btn ref="upload" :files.sync="form.files" :file-classify="fileClassifyEnum.OTHER.V" />
        </el-form-item>
      </div>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 6, maxRows: 8}"
          placeholder="可填写备注"
          style="max-width: 500px;"
        />
      </el-form-item>
    </el-form>
  </el-drawer>
</template>

<script>
import CRUD, { form } from '@crud/crud'
// import branchCompanyButtonPopover from '@/views/components/base/branch-company-button-popover'
// import bankAccountButtonPopover from '@/views/components/base/bank-account-button-popover'
import { tableValidate } from '@/utils/other'
import { paymentFineModeEnum, invoiceTypeEnum, fileClassifyEnum } from '@/utils/enum/index'
import { validatorNatural } from '@/utils/validatePattern'
import { getContractFinanceSimpleInfo } from '@/api/contract/info'
import { getBranchCompanyAllSimple as getAll } from '@/api/common'
import UploadBtn from '@/components/FileUpload/UploadBtn'

const defaultForm = {
  id: undefined,
  payer: '', // 付款方
  paymentFineMode: '', // 付款方式
  paymentReason: '', // 付款事由
  paymentDate: '', // 日期
  paymentAmount: undefined, // 金额
  collectionCompany: '', // 收款方
  collectionDepositBank: '', // 收款开户行
  collectionBankAccount: '', // 收款账户
  remark: '', // 备注
  acceptanceDrafts: []
}

export default {
  components: { UploadBtn },
  mixins: [form(defaultForm)],
  data() {
    return {
      route: this.$route,
      prepared: CRUD.STATUS.PREPARED,
      paymentFineModeEnum,
      invoiceTypeEnum,
      fileClassifyEnum,
      customerName: undefined, // 客户名称
      collectionCompany: undefined,
      rules: {
        payer: [
          { required: true, message: '请填写付款方名称', trigger: 'blur' },
          { max: 60, message: '长度不超过 60 个字符', trigger: 'blur' }
        ],
        paymentFineMode: [
          { required: true, message: '请选择付款方式', trigger: 'change' }
        ],
        paymentReason: [
          { required: true, message: '请选择付款事由', trigger: 'change' }
        ],
        paymentAmount: [
          { required: true, message: '请填写付款金额', trigger: 'change', type: 'number' }
        ],
        paymentDate: [
          { required: true, message: '请选择收款日期', trigger: 'change' }
        ],
        collectionCompany: [
          { required: false, message: '请填写收款方', trigger: 'blur' }
        ],
        collectionDepositBank: [
          { required: false, max: 50, message: '请填写开户行', trigger: 'blur' },
          { max: 50, message: '不能超过 50 个字符', trigger: 'blur' }
        ],
        collectionBankAccount: [
          { required: false, message: '请填写银行账号', trigger: 'blur' },
          { max: 30, pattern: validatorNatural, message: '请输入30位以内的数字', trigger: 'blur' }
        ],
        remark: [{ max: 500, message: '不能超过500个字符', trigger: 'blur' }]
      },
      tableRules: {
        amount: [{ required: true, message: '请填写承兑面额', trigger: 'blur', type: 'number' }],
        discount: [{ required: true, message: '请填写贴现利息', trigger: 'blur', type: 'number' }]
      },
      project: {},
      companyList: []
    }
  },
  mounted() {
    this.$BUS.$on('collectionExtraInfo', ({ customerName, signingMainBodyName }) => {
      this.customerName = customerName
      this.collectionCompany = signingMainBodyName
    })
    this.fetch()
  },
  beforeDestroy() {
    this.$BUS.$off('collectionExtraInfo')
  },
  methods: {
    async fetch() {
      try {
        const { content = [] } = await getAll() || {}
        this.companyList = content
      } catch (error) {
        console.log('获取分公司列表', error)
      }
    },
    [CRUD.HOOK.afterToAdd](crud, form) {
      this.$set(form, 'payer', this.customerName)
      this.$set(form, 'collectionCompany', this.collectionCompany)
      this.$set(form, 'projectId', this.crud.query.projectId)
      this.addRow()
    },
    [CRUD.HOOK.afterValidateCU]() {
      return this.validateTable()
    },
    async handleProjectChange(val) {
      let project = {}
      try {
        if (!val) {
          throw new Error('没有项目id')
        }
        this.projectInfoLoading = true
        project = await getContractFinanceSimpleInfo(val) || {}
      } catch (error) {
        console.log('获取额外项目信息', error)
      } finally {
        this.project = project
        this.projectInfoLoading = false
        if (this.form.amount) {
          this.calcProportion()
        }
      }
    },
    handleCompanyChange(val) {
      this.form.collectionCompany = val.name
      this.form.collectionDepositBank = undefined
      this.form.collectionBankAccount = undefined
      this.$refs.form.validateField('collectionCompany')
    },
    handleDepositBankChange(val) {
      this.form.collectionDepositBank = val.depositBank
      this.form.collectionBankAccount = val.account
      this.$refs.form.validateField('collectionDepositBank')
      this.$refs.form.validateField('collectionBankAccount')
    },
    validateTable() {
      let flag = true
      const list = this.form.acceptanceDrafts
      let message = '请修正表格中标红的信息, 填写限制请参照标题旁边的“信息符号(i)”'
      // 当为承兑汇票时
      if (this.form.paymentFineMode === paymentFineModeEnum.ACCEPTANCE_DRAFT.V) {
        if (list && list.length > 0) {
          let totalAmount = 0
          list.forEach(row => {
            for (const rule in this.tableRules) {
              row.verify = row.verify ? row.verify : {}
              row.verify[rule] = tableValidate(this.tableRules[rule], row[rule])
              if (!row.verify[rule]) {
                flag = false
              }
            }
            totalAmount += row.amount
          })
          if (totalAmount > this.form.paymentAmount) {
            flag = false
            message = '承兑面额超过付款金额'
          }
        }
      }
      if (!flag) {
        this.form.acceptanceDrafts = Object.assign([], list)
        this.$message({ message, type: 'error' })
      }
      return flag
    },
    // 添加行
    addRow: function() {
      this.form.acceptanceDrafts.push({ verify: {}})
    },
    // 删除行
    deleteRow: function(index) {
      this.form.acceptanceDrafts.splice(index, 1)
    },
    handelCellClassName({ row, column, rowIndex, columnIndex }) {
      let flag = true
      if (row.verify && Object.keys(row.verify) && Object.keys(row.verify).length > 0) {
        if (row.verify[column.property] === false) {
          flag = tableValidate(this.tableRules[column.property], row[column.property])
        }
        if (flag) {
          row.verify[column.property] = true
        }
      }
      return flag ? '' : 'mask-td'
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
>>>.el-input-number .el-input__inner {
  text-align: left;
}
>>>.el-table__row .el-input-number .el-input__inner {
  text-align: right;
}
>>>.el-drawer__header {
  margin-bottom: 10px;
}
.el-divider__text {
  .title {
    background-color: #ffba00;
  }
}
.el-drawer-container {
  padding: 0 20px 20px 20px;
  overflow: auto;
}
.add-row-box {
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  margin-top: 20px;
}
.table-box {
  box-sizing: border-box;
  padding: 0 25px;
}
</style>
