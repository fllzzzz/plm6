<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="650px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
      <div class="form-row" style="display: flex">
        <el-form-item label="单体名称" prop="name">
          <el-input v-model="form.name" type="text" placeholder="请填写单体名称" style="width: 270px" @blur="form.name=form.name.replace(/[ ]/g,'')"/>
        </el-form-item>
        <el-form-item label-width="5px" prop="date">
          <el-date-picker
            v-model="form.date"
            type="date"
            value-format="x"
            placeholder="选择完成日期"
            style="width: 160px"
            :disabledDate="mainDateOptionFn"
          />
        </el-form-item>
      </div>
      <div v-for="item in currentOption" :key="item.key" class="form-row" style="display: flex">
        <el-form-item :label="item.label+'('+item.unit+')'" :prop="item.key">
          <el-input-number
            v-model="form[item.key]"
            :step="10"
            :min="0"
            :max="999999999999"
            :precision="item.no===TechnologyTypeAllEnum.STRUCTURE.V?DP.COM_WT__KG:DP.MES_ENCLOSURE_L__M"
            controls-position="right"
            style="width: 270px"
          />
        </el-form-item>
        <el-form-item label-width="5px" :prop="item.dateKey">
          <el-date-picker
            v-model="form[item.dateKey]"
            type="date"
            value-format="x"
            placeholder="选择完成日期"
            style="width: 160px"
            :disabledDate="subDateOptionFn"
            :disabled="!form[item.key]"
          />
        </el-form-item>
      </div>
      <el-form-item label="排序" prop="sort">
        <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
      </el-form-item>
      <el-form-item label="备注" prop="remark">
        <el-input
          v-model="form.remark"
          type="textarea"
          :autosize="{ minRows: 4, maxRows: 6 }"
          placeholder="请填写备注"
          :maxlength="200"
          show-word-limit
          style="width: 320px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { regForm } from '@compos/use-crud'
import { isNotBlank } from '@data-type/index'
import { DP } from '@/settings/config'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'
import { ElMessage } from 'element-plus'

const formRef = ref()
const currentOption = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  },
  originOption: {
    type: Array,
    default: () => []
  },
  globalProject: {
    type: Object,
    default: () => {}
  }
})
const defaultForm = {
  id: undefined,
  projectId: undefined,
  name: '',
  date: undefined,
  sort: 1,
  remark: '',
  battenBoard: undefined,
  battenBoardDate: undefined,
  contourPlate: undefined,
  contourPlateDate: undefined,
  flangingPiece: undefined,
  flangingPieceDate: undefined,
  mainStructure: undefined,
  mainStructureDate: undefined,
  pressureBearingPlate: undefined,
  pressureBearingPlateDate: undefined,
  subStructure: undefined,
  subStructureDate: undefined,
  trussFloorPlate: undefined,
  trussFloorPlateDate: undefined,
  detailSaveDTOParamList: undefined
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const checkDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择完成日期'))
  } else {
    if (props.globalProject.endDate && value > props.globalProject.endDate) {
      callback(new Error('不能超过项目完成时间'))
    } else {
      callback()
    }
  }
}
// const checkOtherDate = (rule, value, callback) => {
//   if (!value) {
//     callback(new Error('请选择完成日期'))
//   } else {
//     if (crud.form.date && value > crud.form.date) {
//       callback(new Error('不能超过单体完成时间'))
//     } else {
//       callback()
//     }
//   }
// }

const checkOtherDate = (rule, value, callback) => {
  if (value) {
    if (crud.form.date && value > crud.form.date) {
      callback(new Error('不能超过单体完成时间'))
    } else {
      callback()
    }
  }
  callback()
}
const rules = {
  date: [{ validator: checkDate, trigger: 'change' }],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  battenBoard: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  battenBoardDate: [{ validator: checkOtherDate, trigger: 'change' }],
  contourPlate: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  contourPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  flangingPiece: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  flangingPieceDate: [{ validator: checkOtherDate, trigger: 'change' }],
  mainStructure: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  mainStructureDate: [{ validator: checkOtherDate, trigger: 'change' }],
  pressureBearingPlate: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  pressureBearingPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  subStructure: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  subStructureDate: [{ validator: checkOtherDate, trigger: 'change' }],
  trussFloorPlate: [{ message: '请填写', trigger: 'blur', type: 'number' }],
  trussFloorPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  name: [
    { required: true, message: '请填写单体名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }]
}

watch(
  () => props.globalProject,
  (val) => {
    if (isNotBlank(val)) {
      currentOption.value = []
      props.originOption.forEach(v => {
        if (val.projectContentList.findIndex((k) => k.no === v.no) > -1) {
          currentOption.value.push(v)
        }
      })
    }
  },
  { deep: true, immediate: true }
)
function mainDateOptionFn(time) {
  if (props.globalProject.endDate) {
    return time.getTime() - 8.64e6 > props.globalProject.endDate || time.getTime() < props.globalProject.startDate
  } else {
    return time.getTime() < props.globalProject.startDate
  }
}

function subDateOptionFn(time) {
  if (crud.form.date) {
    return time.getTime() - 8.64e6 > crud.form.date || time.getTime() < props.globalProject.startDate
  } else {
    return time.getTime() - 8.64e6 > props.globalProject.endDate || time.getTime() < props.globalProject.startDate
  }
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.detailSaveDTOParamList = []
  currentOption.value.forEach((v) => {
    let val
    if (crud.form.id) {
      val = crud.form.monomerDetailList.find(k => k.type === v.no)
    }
    if (crud.form[v.key]) {
      if (val) {
        val.mete = crud.form[v.key]
        val.date = crud.form[v.dateKey]
        crud.form.detailSaveDTOParamList.push(val)
      } else {
        crud.form.detailSaveDTOParamList.push({
          mete: crud.form[v.key],
          date: crud.form[v.dateKey],
          type: v.no
        })
      }
    }
  })
  let flag = true
  crud.form.detailSaveDTOParamList.map(v => {
    if (!v.date) {
      flag = false
    }
  })
  if (!flag) {
    ElMessage.error('完成日期必填')
    return false
  }
  crud.form.projectId = props.projectId
  return !!crud.form.projectId
}
</script>
<style scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

