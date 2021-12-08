<template>
  <el-dialog
    append-to-body
    :visible.sync="visible"
    :close-on-click-modal="false"
    top="10vh"
    width="600px"
    :before-close="handleClose"
  >
    <div slot="title">
      <span>项目结算</span>
      <el-button v-if="$isNotBlank(auditStaus)" size="mini" :type="auditStaus==1?'info':(auditStaus==2?'success':'warning')">
        {{ auditStaus==1?'已驳回':(auditStaus==2?'已通过':'审核中') }}
      </el-button>
    </div>
    <el-dialog
      width="500px"
      :title="'说明'"
      :close-on-click-modal="false"
      :visible.sync="innerVisible"
      append-to-body
    >
      <el-form ref="innerform" :model="innerform" :rules="rules1" size="small" label-width="90px">
        <el-form-item label="原因描述" prop="remark">
          <el-input
            v-model.trim="innerform.remark"
            type="textarea"
            :autosize="{ minRows: 4, maxRows: 6}"
            placeholder="请填写原因描述"
            style="width: 320px;"
          />
        </el-form-item>
      </el-form>
      <div slot="footer" class="dialog-footer">
        <el-button size="small" plain @click="handleCloseInner">取消</el-button>
        <el-button v-if="innerform.status===1" size="small" type="info" @click="handleCloseInner">驳回</el-button>
        <el-button v-if="innerform.status===2" size="small" type="success" @click="handleCloseInner">审核通过</el-button>
      </div>
    </el-dialog>
    <el-form ref="form" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="合同编号" prop="contractNo">
        <el-input
          v-model="form.contractNo"
          placeholder="合同编号"
          style="width: 420px;"
          disabled
        />
      </el-form-item>
      <el-form-item label="项目名称" prop="contractNo">
        <el-input
          v-model="form.name"
          placeholder="项目名称"
          style="width: 420px;"
          disabled
        />
      </el-form-item>
      <el-form-item label="合同金额" prop="contractNo">
        <el-input
          v-model="form.contractAmount"
          placeholder="合同金额"
          style="width: 420px;"
          disabled
        />
      </el-form-item>
      <el-form-item label="结算金额" prop="contractNo">
        <e-input-number
          v-model="form.amount"
          :step="10000"
          :min="1"
          :max="9999999999"
          :precision="$DP.YUAN"
          controls-position="right"
          placeholder="结算金额(元)"
          style="width: 420px;"
        />
      </el-form-item>
      <el-form-item label="结算差异" prop="contractNo">
        <e-input-number
          v-model="form.amount"
          :step="10000"
          :min="1"
          :max="9999999999"
          :precision="$DP.YUAN"
          controls-position="right"
          placeholder="结算差异"
          disabled
          style="width: 420px;"
        />
      </el-form-item>
      <el-form-item label="结算日期">
        <el-date-picker
          v-model="form.deadline"
          type="date"
          value-format="timestamp"
          placeholder="结算日期"
          style="width: 420px;"
        />
      </el-form-item>
      <el-form-item label="办理人" prop="leaderIds">
        <user-dept-cascader
          :value.sync="form.leaderIds"
          multiple
          filterable
          clearable
          show-all-levels
          placeholder="办理人"
          style="width: 420px;"
        />
      </el-form-item>
      <el-form-item label="附件" prop="files">
        <upload-btn ref="upload" :files.sync="form.files" :file-classify="fileClassifyEnum.OTHER.V" />
      </el-form-item>
    </el-form>
    <div slot="footer" class="dialog-footer">
      <el-button size="small" plain @click="handleClose">取消</el-button>
      <!--      <el-button size="small" type="info" @click="passConfirm(1)">驳回</el-button>-->
      <!--      <el-button size="small" type="success" @click="passConfirm(2)">通过</el-button>-->
      <template v-if="$isNotBlank(auditStaus)">
        <el-button v-if="auditStaus===0" size="small" type="info" @click="passConfirm(1)">驳回</el-button>
        <el-button v-if="auditStaus===0" size="small" type="success" @click="passConfirm(2)">通过</el-button>
      </template>
      <template v-else>
        <el-button slot="reference" type="primary" size="small" @click="onSubmit">提交</el-button>
      </template>
    </div>
  </el-dialog>
</template>

<script>
import { getContractFinanceSimpleInfo } from '@/api/contract/info'
// import CRUD, { form } from '@crud/crud'
import enumOperate, { enabledEnum, engineerSettlementTypeEnum as settlementTypeEnum, fileClassifyEnum } from '@/utils/enum/index'
import userDeptCascader from '@/views/components/base/user-dept-cascader'
import UploadBtn from '@/components/FileUpload/UploadBtn'
import { mapGetters } from 'vuex'
import checkPermission from '@/utils/permission'
import { debounce } from '@/utils'
import { getBaseInfo as getDetail } from '@/api/contract/info'
const settlementTypeEnumV = enumOperate.getVal(settlementTypeEnum)

const defaultForm = {
  id: undefined,
  projectId: '',
  contractNo: undefined,
  name: undefined,
  contractAmount: '',
  deadline: undefined,
  leaderIds: undefined,
  remark: undefined
}
const permission = {
  get: ['contract:detail']
}
export default {
  components: { userDeptCascader, UploadBtn },
  // mixins: [form(defaultForm)],
  props: {
    visible: {
      type: Boolean,
      required: true
    },
    auditStaus: {
      type: [Number, String],
      default: undefined
    },
    projectId: {
      type: [Number, String],
      default: undefined
    }
  },
  data() {
    return {
      fileClassifyEnum,
      form: {
        files: []
      },
      enabledEnum,
      settlementTypeEnum,
      settlementTypeEnumV,
      contractAmount: undefined,
      project: undefined,
      projectInfoLoading: false,
      rules: {
        projectId: [
          { required: true, message: '请选择项目', trigger: 'change' }
        ],
        leaderIds: [
          { required: true, message: '请选择办理人', trigger: 'change' }
        ],
        deadline: [
          { required: true, message: '请选择计划完成时间', trigger: 'change' }
        ],
        amount: [
          { required: true, message: '请填写收款金额', trigger: 'change', type: 'number' }
        ],
        remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
      },
      innerVisible: false,
      innerform: {
        status: 0
      },
      rules1: {}
    }
  },
  computed: {
    ...mapGetters(['globalProjectId']),
    currentProjectId() {
      return this.projectId || this.globalProjectId
    }
  },
  watch: {
    currentProjectId: {
      handler(val) {
        this.fetchDetail()
      }
    }
  },
  created() {
    // this.resetForm()
    this.fetchDetail()
  },
  methods: {
    fetchDetail: debounce(async function() {
      if (!checkPermission(permission.get) || !this.currentProjectId) {
        return
      }
      const loading = this.$loading({
        target: '#pageContainer',
        lock: true,
        text: '请稍后，正在加载合同基础信息',
        fullscreen: false
      })
      let _detail = {}
      try {
        const res = await getDetail(this.currentProjectId)
        _detail = JSON.parse(JSON.stringify(res))
      } catch (error) {
        console.log('error', error)
      } finally {
        this.resetForm(defaultForm)
        this.form.contractNo = _detail.contractNo
        this.form.name = _detail.name
        this.form.contractAmount = _detail.contractAmount
        // this.detail = _detail
        loading.close()
      }
    }, 100, false),
    handleClose() {
      this.resetForm()
      this.$emit('changestaus', false)
    },
    handleCloseInner() {
      this.innerVisible = false
    },
    passConfirm(val) {
      this.innerform.status = val
      this.innerVisible = true
    },
    async onSubmit() {
      try {
        const valid = await this.$refs.form.validate()
        if (valid) {
          this.handleClose()
        }
      } catch (error) {
        console.log('error', error)
      }
    },
    // [CRUD.HOOK.afterToCU](crud, form) {
    //   if (crud.status.add === CRUD.STATUS.PREPARED) {
    //     form.projectId = crud.query.projectId
    //   }
    //   if (crud.status.edit === CRUD.STATUS.PREPARED) {
    //     form.projectId = (form.project && form.project.id)
    //   }
    //   this.handleProjectChange(form.projectId)
    //   if (this.$refs[crud.formName]) {
    //     this.$nextTick(() => {
    //       this.$refs[crud.formName].clearValidate()
    //     })
    //   }
    // },
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
    calcProportion() {
      this.form.proportion = this.form.amount && this.project.contractAmount
        ? +(this.form.amount / this.project.contractAmount * 100).toFixed(this.$DP.ACCOUNTING) : undefined
    },
    calcAmount() {
      this.form.amount = this.form.proportion && this.project.contractAmount
        ? +(this.form.proportion * this.project.contractAmount / 100).toFixed(this.$DP.YUAN) : undefined
    },
    resetForm(data) {
      // 清除表单信息
      if (this.$refs['form']) {
        // TODO: 无法清除问题
        this.$refs['form'].resetFields()
      }
      let form
      if (data && Object.keys(data).length > 0) {
        form = data
      } else {
        form = JSON.parse(JSON.stringify(defaultForm))
      }
      // const form = data || JSON.parse(JSON.stringify(defaultForm))
      const crudFrom = this.form
      for (const key in crudFrom) {
        crudFrom[key] = undefined
      }
      for (const key in form) {
        if (crudFrom.hasOwnProperty(key)) {
          crudFrom[key] = form[key]
        } else {
          this.$set(crudFrom, key, form[key])
        }
      }
    }
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  /deep/ .el-input-number .el-input__inner {
    text-align: left;
  }
</style>
