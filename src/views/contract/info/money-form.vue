<template>
  <common-dialog
    append-to-body
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="合同金额变更"
    :center="false"
  >
    <template #title>
      <span style="margin-right:5px;">合同金额变更</span>
      <common-button v-if="auditStaus" size="mini" :type="auditStaus==auditTypeEnum.ENUM.REJECT.V?'info':(auditStaus==auditTypeEnum.ENUM.PASS.V?'success':'warning')">
          {{ auditStaus==auditTypeEnum.ENUM.REJECT.V?'已驳回':(auditStaus==auditTypeEnum.ENUM.PASS.V?'已通过':'审核中') }}
      </common-button>
      <common-button size="mini"  @click="handleClose">关闭</common-button>
    </template>
    <el-form ref="form" :model="form" :rules="rules" size="small" label-width="110px">
      <el-form-item label="合同编号" prop="serialNumber">
        <el-input
          v-if="!auditStaus"
          v-model="contractInfo.serialNumber"
          placeholder="合同编号"
          style="width: 420px;"
          disabled
        />
        <span v-else>{{ contractInfo.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="项目名称" prop="contractNo">
        <el-input
          v-if="!auditStaus"
          v-model="contractInfo.name"
          placeholder="项目名称"
          style="width: 420px;"
          disabled
        />
        <span v-else>{{ contractInfo.name }}</span>
      </el-form-item>
      <el-form-item label="合同金额" prop="contractNo">
        <el-input
          v-if="!auditStaus"
          v-model="contractInfo.contractAmount"
          placeholder="项目金额"
          style="width: 420px;"
          disabled
        />
        <span v-else>{{ contractInfo.contractAmount }}</span>
      </el-form-item>
      <el-form-item label="变更内容" prop="changeContent">
        <el-input
          v-if="!auditStaus"
          v-model="form.changeContent"
          placeholder="变更内容"
          style="width: 420px;"
        />
        <span v-else>{{ form.changeContent }}</span>
      </el-form-item>
      <el-form-item label="变更金额" prop="changeAmount">
        <el-input-number
          v-if="!auditStaus"
          v-model="form.changeAmount"
          :step="100"
          :max="9999999999"
          :precision="DP.YUAN"
          controls-position="right"
          placeholder="变更金额(元)"
          style="width: 420px;"
        />
        <span v-else>{{ form.changeAmount }}</span>
      </el-form-item>
      <el-form-item label="变更后合同金额（元）" prop="contractNo">
        <el-input-number
          v-if="!auditStaus"
          v-model="newAmount"
          :step="10000"
          :min="1"
          :max="9999999999"
          :precision="DP.YUAN"
          controls-position="right"
          placeholder="变更合同金额"
          disabled
          style="width: 420px;"
        />
        <span v-else>{{ newAmount }}</span>
      </el-form-item>
      <el-form-item label="变更日期" prop="changeDate">
        <el-date-picker
          v-if="!auditStaus"
          v-model="form.changeDate"
          type="date"
          value-format="x"
          placeholder="变更日期"
          style="width: 420px;"
        />
        <span v-else v-parse-time="'{y}-{m}-{d}'">{{ form.changeDate }}</span>
      </el-form-item>
      <el-form-item label="负责人" prop="leaderIds">
        <user-dept-cascader
          v-if="!auditStaus"
          v-model="form.userList"
          multiple
          filterable
          clearable
          show-all-levels
          placeholder="负责人"
          style="width: 420px;"
        />
        <span v-else>{{ form.userList }}</span>
      </el-form-item>
      <el-form-item label="描述" prop="remark">
        <el-input
          v-if="!auditStaus"
          v-model="form.changeDesc"
          type="textarea"
          :autosize="{ minRows: 8, maxRows: 8}"
          placeholder="请填写描述"
          style="width: 420px;"
        />
        <span v-else>{{ form.changeDesc }}</span>
      </el-form-item>
      <el-form-item label="附件">
        <upload-btn v-if="!auditStaus" ref="upload" v-model:files="form.files" :file-classify="fileClassifyEnum.CONTRACT_ATT.V" />
        <span v-else />
      </el-form-item>
    </el-form>
    <div slot="footer" class="dialog-footer">
      <common-button size="small" plain @click="handleClose">取消</common-button>
      <template v-if="auditStaus">
        <common-button v-if="auditStaus==auditTypeEnum.ENUM.AUDITING.V" size="small" type="info" @click="passConfirm(auditTypeEnum.ENUM.REJECT.V)">驳回</common-button>
        <common-button v-if="auditStaus==auditTypeEnum.ENUM.AUDITING.V" size="small" type="success" @click="passConfirm(auditTypeEnum.ENUM.PASS.V)">通过</common-button>
      </template>
      <template v-else>
        <common-button slot="reference" type="primary" size="small" @click="onSubmit">提交</common-button>
      </template>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, computed, watch } from 'vue'
import { auditTypeEnum } from '@enum-ms/contract'
import { fileClassifyEnum } from '@enum-ms/file'
import useVisible from '@compos/use-visible'
import userDeptCascader from '@comp-base/user-dept-cascader.vue'
import UploadBtn from '@/components/file-upload/UploadBtn'
import { DP } from '@/settings/config'
const props=defineProps({
  projectId: [Number, String],
  auditStaus: [Number, String],
  modelValue: {
    type: Boolean,
    require: true
  },
  contractInfo:{
    type: Object,
    default: () => {}
  }
})

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

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const rules = {

}
const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const newAmount = computed(()=>{
  if(props.contractInfo.contractAmount && form.changeAmount){
    return props.contractInfo.contractAmount+form.changeAmount
  }
})
function onSubmit(){

}
// import { getContractFinanceSimpleInfo } from '@/api/contract/info'
// import enumOperate, { enabledEnum, engineerSettlementTypeEnum as settlementTypeEnum, fileClassifyEnum } from '@/utils/enum/index'
// import userDeptCascader from '@/views/components/base/user-dept-cascader'
// import { mapGetters } from 'vuex'
// import UploadBtn from '@/components/FileUpload/UploadBtn'
// import checkPermission from '@/utils/permission'
// import { debounce } from '@/utils'
// import { getBaseInfo as getDetail } from '@/api/contract/info'
// const settlementTypeEnumV = enumOperate.getVal(settlementTypeEnum)

// const defaultForm = {
//   id: undefined,
//   projectId: '',
//   contractNo: undefined,
//   name: undefined,
//   contractAmount: '',
//   deadline: undefined,
//   leaderIds: undefined,
//   remark: undefined
// }
// // crud交由presenter持有
// const permission = {
//   get: ['contract:detail']
// }

// export default {
//   components: { userDeptCascader, UploadBtn },
//   // mixins: [form(defaultForm)],
//   props: {
//     visible: {
//       type: Boolean,
//       required: true
//     },
//     auditStaus: {
//       type: [Number, String],
//       default: undefined
//     },
//     projectId: {
//       type: [Number, String],
//       default: undefined
//     }
//   },
//   data() {
//     return {
//       fileClassifyEnum,
//       form: {
//         files: []
//       },
//       checkStatus: 0,
//       enabledEnum,
//       settlementTypeEnum,
//       settlementTypeEnumV,
//       contractAmount: undefined,
//       project: undefined,
//       projectInfoLoading: false,
//       innerVisible: false,
//       rules: {
//         projectId: [
//           { required: true, message: '请选择项目', trigger: 'change' }
//         ],
//         leaderIds: [
//           { required: true, message: '请选择负责人', trigger: 'change' }
//         ],
//         deadline: [
//           { required: true, message: '请选择计划完成时间', trigger: 'change' }
//         ],
//         amount: [
//           { required: true, message: '请填写收款金额', trigger: 'change', type: 'number' }
//         ],
//         remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
//       },
//       innerform: {
//         status: 0
//       },
//       rules1: {}
//       // avatar: ''
//     }
//   },
//   computed: {
//     ...mapGetters(['globalProjectId']),
//     currentProjectId() {
//       return this.projectId || this.globalProjectId
//     },
//     isEditing() {
//       return this.visible
//     }
//   },
//   watch: {
//     currentProjectId: {
//       handler(val) {
//         this.fetchDetail()
//       }
//     }
//   },
//   created() {
//     this.fetchDetail()
//   },
//   methods: {
//     fetchDetail: debounce(async function() {
//       if (!checkPermission(permission.get) || !this.currentProjectId) {
//         return
//       }
//       const loading = this.$loading({
//         target: '#pageContainer',
//         lock: true,
//         text: '请稍后，正在加载合同基础信息',
//         fullscreen: false
//       })
//       let _detail = {}
//       try {
//         const res = await getDetail(this.currentProjectId)
//         _detail = JSON.parse(JSON.stringify(res))
//       } catch (error) {
//         console.log('error', error)
//       } finally {
//         this.resetForm(defaultForm)
//         this.form.contractNo = _detail.contractNo
//         this.form.name = _detail.name
//         this.form.contractAmount = _detail.contractAmount
//         // this.detail = _detail
//         loading.close()
//       }
//     }, 100, false),
//     handleClose() {
//       this.$emit('changestaus', false)
//     },
//     handleCloseInner() {
//       this.innerVisible = false
//     },
//     passConfirm(val) {
//       this.innerform.status = val
//       this.innerVisible = true
//     },
//     async onSubmit() {
//       try {
//         const valid = await this.$refs.form.validate()
//         if (valid) {
//           this.handleClose()
//         }
//       } catch (error) {
//         console.log('error', error)
//       }
//     },
//     // [CRUD.HOOK.afterToCU](crud, form) {
//     //   if (crud.status.add === CRUD.STATUS.PREPARED) {
//     //     form.projectId = crud.query.projectId
//     //   }
//     //   if (crud.status.edit === CRUD.STATUS.PREPARED) {
//     //     form.projectId = (form.project && form.project.id)
//     //   }
//     //   this.handleProjectChange(form.projectId)
//     //   if (this.$refs[crud.formName]) {
//     //     this.$nextTick(() => {
//     //       this.$refs[crud.formName].clearValidate()
//     //     })
//     //   }
//     // },
//     async handleProjectChange(val) {
//       let project = {}
//       try {
//         if (!val) {
//           throw new Error('没有项目id')
//         }
//         this.projectInfoLoading = true
//         project = await getContractFinanceSimpleInfo(val) || {}
//       } catch (error) {
//         console.log('获取额外项目信息', error)
//       } finally {
//         this.project = project
//         this.projectInfoLoading = false
//         if (this.form.amount) {
//           this.calcProportion()
//         }
//       }
//     },
//     calcProportion() {
//       this.form.proportion = this.form.amount && this.project.contractAmount
//         ? +(this.form.amount / this.project.contractAmount * 100).toFixed(this.$DP.ACCOUNTING) : undefined
//     },
//     calcAmount() {
//       this.form.amount = this.form.proportion && this.project.contractAmount
//         ? +(this.form.proportion * this.project.contractAmount / 100).toFixed(this.$DP.YUAN) : undefined
//     },
//     resetForm(data) {
//       // 清除表单信息
//       if (this.$refs['form']) {
//         // TODO: 无法清除问题
//         this.$refs['form'].resetFields()
//       }
//       let form
//       if (data && Object.keys(data).length > 0) {
//         form = data
//       } else {
//         form = JSON.parse(JSON.stringify(defaultForm))
//       }
//       // const form = data || JSON.parse(JSON.stringify(defaultForm))
//       const crudFrom = this.form
//       for (const key in crudFrom) {
//         crudFrom[key] = undefined
//       }
//       for (const key in form) {
//         if (crudFrom.hasOwnProperty(key)) {
//           crudFrom[key] = form[key]
//         } else {
//           this.$set(crudFrom, key, form[key])
//         }
//       }
//     }
//   }
// }
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
</style>
