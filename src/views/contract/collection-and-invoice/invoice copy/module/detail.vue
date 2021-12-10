<template>
  <el-drawer
    :visible="dlgVisible"
    :before-close="handleClose"
    :wrapper-closable="true"
    direction="rtl"
    size="50%"
    append-to-body
  >
    <template slot="title">
      <div>
        <span>{{ type===1?'开票信息':'开票审核' }}</span>
        <el-button size="mini" :type="detail.auditStaus===1?'info':(detail.auditStaus==2?'success':'warning')">
          {{ detail.auditStaus===1?'已驳回':(detail.auditStaus===2?'已通过':'审核中') }}
        </el-button>
      </div>
      <template v-if="type===2">
        <el-popconfirm
          :title="`确认驳回？`"
          placement="bottom-end"
          style="margin-right:10px;"
        >
          <el-button slot="reference" size="mini" type="info" plain>驳回</el-button>
        </el-popconfirm>
        <el-popconfirm
          :title="`确认通过？`"
          placement="bottom-end"
        >
          <el-button slot="reference" type="success" size="mini">通过</el-button>
        </el-popconfirm>
      </template>
      <template v-else>
        <el-button v-if="!edit" slot="reference" type="primary" size="mini" @click="edit=true">重新编辑</el-button>
        <template v-else>
          <el-button slot="reference" size="mini" type="info" plain style="margin-right:10px;">取消</el-button>
          <el-popconfirm
            :title="`确认通过？`"
            placement="bottom-end"
          >
            <el-button slot="reference" type="success" size="mini">提交</el-button>
          </el-popconfirm>
        </template>
      </template>
    </template>
    <div class="el-drawer-container">
      <el-form ref="detail" :model="detail" size="small" label-width="150px">
        <div class="form-row" style="display:flex;">
          <el-form-item label="项目" prop="projectId">
            <switch-project
              v-if="edit===true"
              :value.sync="detail.projectId"
              :initial="false"
              style="width:250px"
              class="filter-item"
              @change="handleProjectChange"
            />
            <span v-else>测试项目</span>
          </el-form-item>
          <el-form-item label="发票类型" prop="invoiceType">
            <common-select
              v-if="edit===true"
              :value.sync="detail.invoiceType"
              :options="invoiceTypeEnum"
              type="enum"
              size="small"
              placeholder="请选择发票类型"
              style="width: 250px;"
            />
            <span v-else>增值税专用发票</span>
          </el-form-item>
        </div>
        <div class="form-row" style="display:flex;">
          <el-form-item label="合同金额(元)">
            <el-input
              v-if="edit===true"
              v-model="detail.contractAmount"
              type="text"
              placeholder="合同金额"
              style="width: 250px;"
              disabled
            />
            <span v-else>10000</span>
          </el-form-item>
          <el-form-item label="销项税额" prop="managementFeeRate">
            <template v-if="edit===true">
              <el-input
                v-model="detail.contractAmount1"
                :readonly="true"
                style="width:110px"
                placeholder="先输入税率"
              />
              <e-input-number
                v-model="detail.managementFeeRate"
                :step="1"
                :min="0"
                :max="100"
                :precision="$DP.ACCOUNTING"
                :controls="false"
                controls-position="right"
                style="width:80px"
                placeholder="0-100"
              />%
            </template>
            <template v-else>
              <span style="margin-right:5px;">1300</span><span>13%</span>
            </template>
          </el-form-item>
        </div>
        <div class="form-row" style="display:flex;">
          <el-form-item label="开票单位" prop="collectionCompany">
            <el-select v-if="edit===true" v-model="detail.collectionCompany" placeholder="请选择" style="width:250px;">
              <el-option
                v-for="item in companyList"
                :key="item.id"
                :label="item.name"
                :value="item.id"
              />
            </el-select>
            <span v-else>初鸣</span>
          </el-form-item>
          <el-form-item label="发票日期" prop="paymentDate">
            <el-date-picker
              v-if="edit===true"
              v-model="detail.paymentDate"
              type="date"
              value-format="timestamp"
              placeholder="选择发票日期"
              style="width: 250px;"
            />
            <span v-else>2021-10-18</span>
          </el-form-item>
        </div>
        <div class="form-row" style="display:flex;">
          <el-form-item label="收票单位" prop="collectionBankAccount1">
            <el-input
              v-if="edit===true"
              v-model="detail.collectionBankAccount1"
              type="text"
              placeholder="收票单位"
              style="width: 250px;"
            />
            <span v-else>测试</span>
          </el-form-item>
          <el-form-item label="发票号码" prop="collectionBankAccount1">
            <el-input
              v-if="edit===true"
              v-model="detail.collectionBankAccount1"
              type="text"
              placeholder="发票号码"
              style="width: 250px;"
            />
            <span v-else>12345</span>
          </el-form-item>
        </div>
        <div class="form-row" style="display:flex;">
          <el-form-item label="发票面额(元)" prop="paymentAmount">
            <el-input-number
              v-if="edit===true"
              v-model.number="detail.paymentAmount"
              :min="-99999999999"
              :max="99999999999"
              :step="10000"
              :precision="$DP.YUAN"
              placeholder="发票面额"
              controls-position="right"
              style="width: 250px;"
            />
            <span v-else>10000</span>
          </el-form-item>
          <el-form-item label="附件">
            <upload-btn v-if="edit===true" ref="upload" :files.sync="detail.files" :file-classify="fileClassifyEnum.OTHER.V" />
            <span v-else>xx.png</span>
          </el-form-item>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-if="edit===true"
            v-model="detail.remark"
            type="textarea"
            :autosize="{ minRows: 6, maxRows: 8}"
            placeholder="可填写备注"
            style="max-width: 500px;"
          />
          <span v-else>备注</span>
        </el-form-item>
      </el-form>
    </div>
  </el-drawer>
</template>

<script>
import { judgeSameValue } from '@/utils'
import { paymentFineModeEnum, invoiceTypeEnum, fileClassifyEnum } from '@/utils/enum/index'
import { getContractFinanceSimpleInfo } from '@/api/contract/info'
import { getBranchCompanyAllSimple as getAll } from '@/api/common'
import UploadBtn from '@/components/FileUpload/UploadBtn'

export default {
  components: { UploadBtn },
  props: {
    visible: {
      type: Boolean,
      required: true
    },
    record: {
      type: Object,
      required: true
    },
    type: {
      type: [Number, String],
      default: undefined
    }
  },
  data() {
    return {
      paymentFineModeEnum,
      invoiceTypeEnum,
      fileClassifyEnum,
      detail: {},
      projectInfoLoading: false,
      project: {},
      companyList: [],
      edit: false
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
  mounted() {
    this.$BUS.$on('collectionExtraInfo', ({ customerName, signingMainBodyName }) => {
      this.customerName = customerName
      this.collectionCompany = signingMainBodyName
    })
    this.fetch()
  },
  methods: {
    init(record) {
      this.detail = { ...record }
    },
    async fetch() {
      try {
        const { content = [] } = await getAll() || {}
        this.companyList = content
      } catch (error) {
        console.log('获取分公司列表', error)
      }
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
    handleClose() {
      this.$emit('update:visible', false)
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
>>>.el-input-number .el-input__inner {
  text-align: left;
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
.form-row {
  width:100%;
  .el-form-item{
    width:50%;
  }
}
</style>
