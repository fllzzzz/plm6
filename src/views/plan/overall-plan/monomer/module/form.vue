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
          <el-input v-model="form.name" type="text" placeholder="请填写单体名称" style="width: 270px" />
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
        <el-form-item :label="item.label" :prop="item.key">
          <el-input-number
            v-model="form[item.key]"
            :step="10"
            :min="0"
            :max="999999999999"
            :precision="DP.COM_WT__KG"
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
          style="width: 320px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import IconSelect from '@comp/iconSelect/index.vue'
import { isNotBlank } from '@data-type/index'
import { DP } from '@/settings/config'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'

const formRef = ref()
const originOption = [
  { label: '构件(t)', key: 'mainStructure', dateKey: 'mainStructureDate', no: TechnologyTypeAllEnum.ENUM.STRUCTURE.V, alias: 'STRUCTURE' },
  {
    label: '压型板(t)',
    key: 'contourPlate',
    dateKey: 'contourPlateDate',
    no: TechnologyTypeAllEnum.ENUM.PROFILEDPLATE.V,
    alias: 'ENCLOSURE',
  },
  {
    label: '桁架楼承板(t)',
    key: 'trussFloorPlate',
    dateKey: 'trussFloorPlateDate',
    no: TechnologyTypeAllEnum.ENUM.TRUSSFLOORPLATE.V,
    alias: 'ENCLOSURE',
  },
  {
    label: '夹芯板(t)',
    key: 'battenBoard',
    dateKey: 'battenBoardDate',
    no: TechnologyTypeAllEnum.ENUM.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE',
  },
  {
    label: '压型楼承板(t)',
    key: 'pressureBearingPlate',
    dateKey: 'pressureBearingPlateDate',
    no: TechnologyTypeAllEnum.ENUM.PRESSUREBEARINGPLATE.V,
    alias: 'ENCLOSURE',
  },
]
const currentOption = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined,
  },
  currentProject: {
    type: Object,
    default: () => {},
  },
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
  detailSaveDTOParamList: undefined,
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const checkDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择完成日期'))
  } else {
    if (props.currentProject.endDate && value > props.currentProject.endDate) {
      callback(new Error('不能超过项目完成时间'))
    } else {
      callback()
    }
  }
}
const checkOtherDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择完成日期'))
  } else {
    if (crud.form.date && value > crud.form.date) {
      callback(new Error('不能超过单体完成时间'))
    } else {
      callback()
    }
  }
}
const rules = {
  date: [{ validator: checkDate, trigger: 'change' }],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  battenBoard: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  battenBoardDate: [{ validator: checkOtherDate, trigger: 'change' }],
  contourPlate: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  contourPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  flangingPiece: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  flangingPieceDate: [{ validator: checkOtherDate, trigger: 'change' }],
  mainStructure: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  mainStructureDate: [{ validator: checkOtherDate, trigger: 'change' }],
  pressureBearingPlate: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  pressureBearingPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  subStructure: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  subStructureDate: [{ validator: checkOtherDate, trigger: 'change' }],
  trussFloorPlate: [{ required: true, message: '请填写', trigger: 'blur', type: 'number' }],
  trussFloorPlateDate: [{ validator: checkOtherDate, trigger: 'change' }],
  name: [
    { required: true, message: '请填写单体名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' },
  ],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }],
}

watch(
  () => props.currentProject,
  (val) => {
    if (isNotBlank(val)) {
      currentOption.value = []
      val.projectContentList.forEach((v) => {
        if (val.businessType === businessTypeEnum.ENUM.MACHINING.V) {
          if (v.no && originOption.findIndex((k) => k.no == v.no) > -1) {
            const optionVal = originOption.find((k) => k.no == v.no)
            currentOption.value.push(optionVal)
          }
        } else if (val.businessType === businessTypeEnum.ENUM.INSTALLATION.V) {
          if (v.childrenList && v.childrenList.length > 0) {
            v.childrenList.forEach((value) => {
              if (value.no && originOption.findIndex((k) => k.no == value.no) > -1) {
                const optionVal = originOption.find((k) => k.no == value.no)
                currentOption.value.push(optionVal)
              }
            })
          }
        }
      })
      if (currentOption.value.findIndex((k) => k.alias === 'ENCLOSURE') > -1) {
        const optionVal = {
          label: '折边件(t)',
          key: 'flangingPiece',
          dateKey: 'flangingPieceDate',
          alias: 'ENCLOSURE',
          no: TechnologyTypeAllEnum.ENUM.BENDING.V,
        }
        currentOption.value.push(optionVal)
      }
    }
  },
  { deep: true, immediate: true }
)
function mainDateOptionFn(time) {
  if (props.currentProject.endDate) {
    return time.getTime() - 8.64e6 > props.currentProject.endDate
  } else {
    return false
  }
}

function subDateOptionFn(time) {
  if (crud.form.date) {
    return time.getTime() - 8.64e6 > crud.form.date
  } else {
    return false
  }
}

CRUD.HOOK.beforeSubmit = () => {
  console.log(currentOption.value)
  crud.form.detailSaveDTOParamList = []
  currentOption.value.forEach((v) => {
    console.log(v.no)
    crud.form.detailSaveDTOParamList.push({
      mete: crud.form[v.key],
      date: crud.form[v.dateKey],
      type: v.no,
    })
  })
  crud.form.projectId = props.projectId
  return !!crud.form.projectId
}
</script>
<style scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

