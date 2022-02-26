<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="handleClose"
    v-model="visible"
    title="清单变更"
    :wrapper-closable="false"
    size="80%"
  >
    <template #titleRight>
      <common-button
        type="primary"
        size="mini"
        @click="onSubmit"
        :disabled="originDetail.shipQuantity>0"
      >确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <div style="margin-bottom:10px;">
          <el-tag size="medium" type="info" effect="dark" style="margin-right:5px;">变更范围</el-tag>
          <template v-if="allArea.length>0">
            <span style="font-size:12px;">
              区域:
              <span v-for="(item,index) in allArea" :key="item.id">{{index!==0?','+item.name:item.name}}</span>
            </span>
          </template>
        </div>
        <el-tag size="medium" type="info" effect="dark">构件信息</el-tag>
        <div style="display: flex; width: 100%">
          <el-form-item label="构件名称">
            <span style="display: inline-block;width: 80px;">{{ originDetail.name }}</span>
          </el-form-item>
          <el-form-item label="编号">
            <span style="display: inline-block;width: 80px;">{{ originDetail.serialNumber }}</span>
          </el-form-item>
          <el-form-item label="清单数量">
            <span style="display: inline-block;width: 80px;">{{ originDetail.quantity }}</span>
          </el-form-item>
          <el-form-item label="生产数量">
            <span style="display: inline-block;width: 80px;">{{ originDetail.productionQuantity }}</span>
          </el-form-item>
          <el-form-item label="发运数量">
            <span style="display: inline-block;width: 80px;">{{ originDetail.shipQuantity }}</span>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="材质">
            <span style="display: inline-block;width: 270px;">{{ originDetail.material }}</span>
          </el-form-item>
          <el-form-item label="单净重(kg)">
            <span style="display: inline-block;width: 270px;">
              {{ form.netWeight?form.netWeight.toFixed(2):'' }}
              <span v-if="form.netWeight!== originDetail.netWeight">
                ({{ originDetail.netWeight }}
                <svg-icon :icon-class="form.netWeight> originDetail.netWeight?'top':'bottom'" :color="form.netWeight> originDetail.netWeight?'green':'red'"/>
                )
              </span>
            </span>
          </el-form-item>
          <el-form-item label="单毛重(kg)">
            <span style="display: inline-block;width: 270px;">
              {{ form.grossWeight?form.grossWeight.toFixed(2):'' }}
              <span v-if="form.grossWeight !== originDetail.grossWeight">
                ({{ originDetail.grossWeight }}
                <svg-icon :icon-class="form.grossWeight> originDetail.grossWeight?'top':'bottom'" :color="form.grossWeight> originDetail.grossWeight?'green':'red'"/>
                )
              </span>
            </span>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="规格" prop="specification">
            <div style="width: 270px">
              <el-input
                v-model="form.specification"
                type="text"
                placeholder="请填写构件规格"
                style="width: 270px"
                maxlength="20"
                :disabled="originDetail.shipQuantity>0"
              />
            </div>
          </el-form-item>
           <el-form-item label="面积(㎡)" prop="newSurfaceArea">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.newSurfaceArea"
                :max="maxNumber"
                :step="1"
                :precision="DP.COM_AREA__M2"
                placeholder="请填写构件面积"
                controls-position="right"
                style="width: 270px"
                :disabled="originDetail.shipQuantity>0"
              />
            </div>
          </el-form-item>
          <el-form-item label="长度(mm)" prop="length">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.length"
                :min="0"
                :max="maxNumber"
                :step="1"
                :precision="DP.MES_ARTIFACT_L__MM"
                placeholder="请填写构件长度"
                controls-position="right"
                style="width: 270px"
                :disabled="originDetail.shipQuantity>0"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display: flex">
          <el-form-item label="图号" prop="drawingNumber">
            <div style="width: 270px">
              <el-input
                v-model="form.drawingNumber"
                type="text"
                placeholder="请填写构件图号"
                style="width: 270px"
                maxlength="20"
                :disabled="originDetail.shipQuantity>0"
              />
            </div>
          </el-form-item>
          <el-form-item label="备注" prop="remark">
            <div style="width: 420px">
              <el-input
                v-model.trim="form.remark"
                type="textarea"
                :autosize="{ minRows: 1, maxRows: 6 }"
                :maxlength="200"
                placeholder="请填写备注"
                style="width: 420px"
                :disabled="originDetail.shipQuantity>0"
              />
            </div>
          </el-form-item>
        </div>
        <div style="height: 35px">
          <el-tag size="medium" type="info" effect="dark" style="float: left">零件信息</el-tag>
          <div style="float: right; margin-top: 10px; margin-right: 20px" v-if="originDetail.shipQuantity<=0">
            <template v-if="!isdisable">
              <common-button size="mini" type="primary" :disabled="editing" @click="handleAdd">新增</common-button>
              <common-button size="mini" type="danger" :disabled="editing" @click="deleteItems">删除</common-button>
              <common-button size="mini" type="success" @click="handleEdit">修改</common-button>
            </template>
            <template v-else>
              <common-button size="mini" @click="closeEdit">取消</common-button>
              <common-button size="mini" type="primary" @click="saveEdit">保存</common-button>
            </template>
          </div>
        </div>
        <div style="padding: 10px 0">
          <common-table
            ref="machinePartRef"
            border
            :data="form.parts"
            :max-height="300"
            style="width: 100%"
            class="table-form"
            :stripe="false"
            :cell-class-name="wrongCellMask"
            @selection-change="handleSelectionChange"
          >
            <el-table-column type="selection" width="55" :disabled="form.changeAbleStatus != 3" />
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column key="serialNumber" prop="serialNumber" label="零件编号" min-width="100">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add"
                  v-model.trim="scope.row.serialNumber"
                  type="text"
                  placeholder="零件编号"
                  size="mini"
                  maxlength="20"
                />
                <span v-else >{{ scope.row.serialNumber }}</span>
              </template>
            </el-table-column>
            <el-table-column key="specification" prop="specification" label="规格" min-width="160">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add"
                  v-model.trim="scope.row.specification"
                  type="text"
                  placeholder="请填写规格"
                  size="mini"
                  maxlength="20"
                />
                <span v-else>{{ scope.row.specification }}</span>
              </template>
            </el-table-column>
            <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="100">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add"
                  v-model.trim="scope.row.material"
                  type="text"
                  placeholder="请填写材质"
                  size="mini"
                  maxlength="20"
                />
                <span v-else>{{ scope.row.material }}</span>
              </template>
            </el-table-column>
            <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`长度\n(mm)`" align="left" min-width="85">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add"
                  v-model.number="scope.row.length"
                  :min="0"
                  :max="maxNumber"
                  :step="1"
                  placeholder="请填写"
                  :precision="DP.MES_ARTIFACT_L__MM"
                  controls-position="right"
                  size="mini"
                />
                <span v-else>{{ scope.row.length ? scope.row.length.toFixed(DP.MES_MACHINE_PART_L__MM) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="netWeight" prop="netWeight" :label="`单净重\n(kg)`" align="left" min-width="80">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add"
                  v-model.number="scope.row.netWeight"
                  :min="0"
                  :max="maxNumber"
                  :step="1"
                  :precision="DP.COM_WT__KG"
                  placeholder="请填写"
                  controls-position="right"
                  size="mini"
                />
                <span v-else>{{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="grossWeight" prop="grossWeight" :label="`单毛重\n(kg)`" align="left" min-width="80">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add"
                  v-model.number="scope.row.grossWeight"
                  :min="0"
                  :max="maxNumber"
                  :step="1"
                  :precision="DP.COM_WT__KG"
                  placeholder="请填写"
                  controls-position="right"
                  size="mini"
                />
                <span v-else>{{ scope.row.grossWeight ? scope.row.grossWeight.toFixed(DP.COM_WT__KG) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="quantity" prop="quantity" label="零件总数" min-width="85">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add || editing"
                  v-model.number="scope.row.quantity"
                  :min="originDetail.quantity"
                  :max="maxNumber"
                  :step="originDetail.quantity"
                  step-strictly
                  placeholder="请填写"
                  controls-position="right"
                  size="mini"
                  @change="partQuantityChange(scope.row)"
                  @blur="partQuantityChange(scope.row)"
                />
                <span v-else >{{ scope.row.quantity }}</span>
              </template>
            </el-table-column>
            <el-table-column key="firstProcessName" prop="firstProcessName" :show-overflow-tooltip="true" :label="originDetail.firstProcessName?'已'+originDetail.firstProcessName:''" align="center" width="100">
              <template v-slot="scope">
                <span>{{ scope.row.unitData && form.producedQuantity?form.producedQuantity*scope.row.unitData: 0 }}</span>
              </template>
            </el-table-column>
          </common-table>
        </div>
        <el-tag size="medium" type="info" effect="dark">变更原因及附件</el-tag>
        <div>
          <div style="float:left;">
            <el-form-item label="原因类型" prop="reasonId">
              <changeRemarkSelect v-model="form.reasonId" clearable :disabled="originDetail.shipQuantity>0"/>
            </el-form-item>
            <el-form-item label="原因描述" prop="changeRemark">
              <el-input
                v-model.trim="form.changeRemark"
                type="textarea"
                :autosize="{ minRows: 3, maxRows: 6 }"
                :maxlength="200"
                placeholder="请填写原因描述"
                :disabled="originDetail.shipQuantity>0"
                style="width: 320px"/>
            </el-form-item>
          </div>
          <div style="float:left;width:60%;">
            <el-form-item label="附件上传" prop="attachmentFiles">
              <upload-list
                ref="uploadRef"
                :show-download="false"
                :file-classify="fileClassifyEnum.CHANGE_LIST_ATT.V"
                v-model:files="form.attachmentFiles"
                empty-text="暂未附件"
                :uploadable="originDetail.shipQuantity<=0"
              />
            </el-form-item>
          </div>
        </div>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, ref, defineEmits, watch, nextTick } from 'vue'
// import { regForm } from '@compos/use-crud'
import { fileClassifyEnum } from '@enum-ms/file'
import { DP } from '@/settings/config'
import useTableValidate from '@compos/form/use-table-validate'
import { ElNotification, ElMessage } from 'element-plus'
import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import changeRemarkSelect from '@comp-base/change-reason-select'
import uploadList from '@comp/file-upload/UploadList.vue'
import { listChange, artifactInfo } from '@/api/plan/technical-manage/artifact-tree'

const formRef = ref()
const editing = ref(false)
const originData = ref([])
const isdisable = ref(false)
const maxNumber = 999999999
const defaultForm = {
  changeReason: undefined,
  drawingNumber: undefined,
  grossWeight: undefined,
  length: undefined,
  material: undefined,
  netWeight: undefined,
  quantity: undefined,
  newQuantity: undefined,
  remark: undefined,
  serialNumber: undefined,
  specification: undefined,
  newSurfaceArea: undefined,
  parts: undefined,
  files: undefined,
  attachmentFiles: [],
  type: undefined
}
const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const machinePartRef = ref()
const choseVal = ref([])
const uploadRef = ref()
const originDetail = ref({})
const assemblyGross = ref()
const assemblyNet = ref()

const props = defineProps({
  allArea: {
    type: Array,
    default: () => []
  },
  modelValue: {
    type: Boolean,
    require: true
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.modelValue,
  (val) => {
    if (val) {
      resetForm()
    }
  },
  { deep: true, immediate: true }
)

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

function resetForm() {
  if (formRef.value) {
    formRef.value.resetFields()
  }
  getInfo(props.detailInfo)
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
}

useWatchFormValidate(formRef, form)

async function getInfo(data) {
  assemblyGross.value = 0
  assemblyNet.value = 0
  originDetail.value = {}
  try {
    let totalPartGross = 0
    let totalPartNet = 0
    originDetail.value = await artifactInfo({ serialNumber: data.serialNumber, monomerId: data.monomerId })
    originDetail.value.newSurfaceArea = originDetail.value.surfaceArea ? originDetail.value.surfaceArea / 1000000 : ''
    if (originDetail.value.parts.length > 0) {
      originDetail.value.parts.map(v => {
        if (originDetail.value.quantity && v.quantity) {
          v.unitData = v.quantity / originDetail.value.quantity
          totalPartGross += v.grossWeight * v.unitData
          totalPartNet += v.netWeight * v.unitData
        }
      })
    }
    form.value = JSON.parse(JSON.stringify(originDetail.value))
    assemblyGross.value = form.value.grossWeight - totalPartGross
    assemblyNet.value = form.value.netWeight - totalPartNet
  } catch (error) {
    console.log('获取构件信息', error)
  }
}

const rules = {
  specification: [
    { required: true, message: '请填写构件规格', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  material: [
    { required: true, message: '请填写构件材质', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  drawingNumber: [{ max: 64, message: '不能超过64个字符', trigger: 'blur' }],
  remark: [{ max: 200, message: '不能超过 200 个字符', trigger: 'blur' }],
  length: [{ required: true, message: '请填写构件长度', trigger: 'blur', type: 'number' }],
  netWeight: [{ required: true, message: '请填写构件净重', trigger: 'blur', type: 'number' }],
  grossWeight: [{ required: true, message: '请填写构件毛重', trigger: 'blur', type: 'number' }],
  newSurfaceArea: [{ message: '请填写构件面积', trigger: 'blur', type: 'number' }],
  type: [{ required: true, message: '请选择原因类型', trigger: 'change' }],
  reasonId: [{ required: true, message: '请选择变更原因', trigger: 'change' }],
  changeRemark: [{ required: true, max: 200, message: '不能超过 200 个字符', trigger: 'blur' }]
}

const tableRules = {
  serialNumber: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  specification: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  quantity: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  length: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  material: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  grossWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  netWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }]
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function handleAdd() {
  originData.value = JSON.parse(JSON.stringify(form.value.parts))
  form.value.parts.unshift({
    grossWeight: undefined,
    id: undefined,
    length: undefined,
    material: undefined,
    netWeight: undefined,
    quantity: undefined,
    remark: undefined,
    serialNumber: undefined,
    specification: undefined,
    add: true,
    unitData: undefined,
    dataIndex: form.value.parts.length
  })
  isdisable.value = true
}
function handleEdit() {
  editing.value = true
  originData.value = JSON.parse(JSON.stringify(form.value.parts))
  isdisable.value = true
}
function closeEdit() {
  editing.value = false
  originData.value.map((v) => {
    v.add = false
  })
  form.value.parts = originData.value
  isdisable.value = false
}
function saveEdit() {
  const { validResult, dealList } = tableValidate(form.value.parts)
  if (validResult) {
    form.value.parts = dealList
  } else {
    return validResult
  }
  editing.value = false
  originData.value.map((v) => {
    v.add = false
  })
  weigthChange()
  isdisable.value = false
}

function weigthChange() {
  let grossWeight = 0
  let netWeight = 0
  form.value.parts.map((v) => {
    if (!v.unitData) {
      v.unitData = v.quantity / form.value.quantity
    }
    grossWeight += v.grossWeight * v.unitData
    netWeight += v.netWeight * v.unitData
    v.add = false
  })
  form.value.grossWeight = grossWeight + assemblyGross.value
  form.value.netWeight = netWeight + assemblyNet.value
}

function handleSelectionChange(val) {
  choseVal.value = val
}

function deleteItems() {
  if (choseVal.value && choseVal.value.length > 0) {
    choseVal.value.forEach((i) => {
      if (i.id) {
        const idIndex = form.value.parts.findIndex((v) => v.id === i.id)
        form.value.parts.splice(idIndex, 1)
      } else {
        const index = form.value.parts.findIndex(v => v.dataIndex === i.dataIndex)
        form.value.parts.splice(index, 1)
      }
    })
  } else {
    ElMessage.error('请先勾选选项')
    return
  }
  weigthChange()
  machinePartRef.value.clearSelection()
}

function partQuantityChange(row) {
  if (row.quantity) {
    row.unitData = row.quantity / form.value.quantity
  }
}

function handleSuccess() {
  ElNotification({ title: '更改成功', type: 'success' })
  emit('success')
  handleClose()
}

async function onSubmit(val) {
  if (form.value.parts.length === 0) {
    ElMessage.error('请填写零件信息')
    return
  }
  try {
    await formRef.value.validate()
    form.value.monomerId = Number(props.detailInfo.monomerId)
    form.value.attachments = form.value.attachmentFiles && form.value.attachmentFiles.length > 0 ? form.value.attachmentFiles.map((v) => v.id) : []
    form.value.surfaceArea = form.value.newSurfaceArea * 1000000 !== form.value.surfaceArea ? form.value.newSurfaceArea * 1000000 : form.value.surfaceArea
    await listChange(form.value)
    handleSuccess()
  } catch (e) {
    console.log('修改构件数量', e)
  }
}
</script>
<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.el-dialog__body) {
  padding: 10px 20px;

  .el-step {
    .el-step__icon {
      width: 20px;
      height: 20px;
      font-size: 12px;
    }
    .el-step__title {
      font-size: 13px;
    }
  }
}
.tree-form {
  ::v-deep(.el-drawer__header) {
    margin-bottom: 0;
  }
}
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 5px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
::v-deep(.el-table--small .el-table__cell){
  padding:2px 0;
}
</style>
