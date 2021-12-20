<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="80%"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <div class="item-name">构件信息</div>
        <div style="display: flex; width: 100%">
          <el-form-item label="构件名称">
            <span>{{ form.name }}</span>
          </el-form-item>
          <el-form-item label="编号">
            <span>{{ form.serialNumber }}</span>
          </el-form-item>
          <el-form-item label="构件数量">
            <span>{{ form.quantity }}</span>
          </el-form-item>
          <el-form-item label="生产数量">
            <span>{{ form.producedQuantity }}</span>
          </el-form-item>
          <el-form-item label="关联组立号">
            <span>{{ form.name }}</span>
          </el-form-item>
        </div>
        <div style="display: flex; width: 100%">
          <el-form-item label="规格" prop="specification">
            <div style="width: 270px">
              <el-input v-model="form.specification" type="text" placeholder="请填写构件规格" style="width: 270px" />
            </div>
          </el-form-item>
          <el-form-item label="材质" prop="material">
            <div style="width: 270px">
              <el-input v-model="form.material" type="text" placeholder="请填写构件材质" style="width: 270px" />
            </div>
          </el-form-item>
          <el-form-item label="单毛重(kg)" prop="grossWeight">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.grossWeight"
                :min="0"
                :max="maxNubmer"
                :step="1"
                :precision="DP.COM_WT__KG"
                placeholder="请填写构件毛重"
                controls-position="right"
                style="width: 270px"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display: flex">
          <el-form-item label="面积(㎡)" prop="area">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.area"
                :max="maxNubmer"
                :step="1"
                :precision="DP.COM_AREA__M2"
                placeholder="请填写构件面积"
                controls-position="right"
                style="width: 270px"
              />
            </div>
          </el-form-item>
          <el-form-item label="长度(mm)" prop="length">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.length"
                :min="0"
                :max="maxNubmer"
                :step="1"
                :precision="DP.MES_ARTIFACT_L__MM"
                placeholder="请填写构件长度"
                controls-position="right"
                style="width: 270px"
              />
            </div>
          </el-form-item>
          <el-form-item label="单净重(kg)" prop="netWeight">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.netWeight"
                :min="0"
                :max="maxNubmer"
                :step="1"
                :precision="DP.COM_WT__KG"
                placeholder="请填写构件净重"
                controls-position="right"
                style="width: 270px"
              />
            </div>
          </el-form-item>
        </div>
        <div style="display: flex">
          <el-form-item label="图号" prop="drawingNumber">
            <div style="width: 270px">
              <el-input v-model="form.drawingNumber" type="text" placeholder="请填写构件图号" style="width: 270px" />
            </div>
          </el-form-item>
          <el-form-item label="构件数量" prop="newQuantity">
            <div style="width: 270px">
              <el-input-number
                v-model.number="form.newQuantity"
                :min="0"
                :max="maxNubmer"
                :step="1"
                step-strictly
                placeholder="不变更无需填写"
                controls-position="right"
                style="width: 270px"
              />
            </div>
          </el-form-item>
          <el-form-item label="备注" prop="remark">
            <div style="width: 270px">
              <el-input
                v-model.trim="form.remark"
                type="textarea"
                :autosize="{ minRows: 1, maxRows: 6 }"
                placeholder="请填写备注"
                style="width: 270px"
              />
            </div>
          </el-form-item>
        </div>
        <div style="height: 50px">
          <div class="item-name" style="float: left">零件信息</div>
          <template v-if="!form.producedQuantity">
            <div style="float: right; margin-top: 10px; margin-right: 20px">
              <template v-if="!isdisable">
                <common-button size="mini" type="primary" :disabled="editing" @click="handleAdd">新增</common-button>
                <common-button size="mini" type="danger" :disabled="editing">删除</common-button>
                <common-button size="mini" type="success" @click="handleEdit">修改</common-button>
              </template>
              <template v-else>
                <common-button size="mini" @click="closeEdit">取消</common-button>
                <common-button size="mini" type="primary" @click="saveEdit">保存</common-button>
              </template>
            </div>
          </template>
        </div>
        <div style="padding: 0 20px">
          <common-table
            ref="table"
            border
            :data="form.children"
            :max-height="300"
            style="width: 100%"
            class="table-form"
            :cell-class-name="wrongCellMask"
            @selection-change="handleSelectionChange"
          >
            <el-table-column type="selection" width="55" />
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column key="serialNumber" prop="serialNumber" label="*零件编号" min-width="100">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add"
                  v-model="scope.row.serialNumber"
                  type="text"
                  placeholder="零件编号"
                  style="min-width: 100px"
                  size="mini"
                />
                <span v-else style="cursor: pointer">{{ scope.row.serialNumber }}</span>
              </template>
            </el-table-column>
            <el-table-column key="specification" prop="specification" label="*规格" min-width="160">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add || editing"
                  v-model="scope.row.specification"
                  type="text"
                  placeholder="请填写构件规格"
                  style="width: 160px"
                  size="mini"
                />
                <span v-else>{{ scope.row.specification }}</span>
              </template>
            </el-table-column>
            <el-table-column key="quantity" prop="quantity" label="*零件总数" min-width="140">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add || editing"
                  v-model.number="scope.row.quantity"
                  :min="0"
                  :max="maxNubmer"
                  :step="form.quantity"
                  step-strictly
                  placeholder="请填写"
                  controls-position="right"
                  style="width: 140px"
                  size="mini"
                />
                <span v-else style="cursor: pointer">{{ scope.row.quantity }}</span>
              </template>
            </el-table-column>
            <el-table-column key="producedQuantity" prop="producedQuantity" :show-overflow-tooltip="true" label="已使用" min-width="100">
              <template v-slot="scope">
                <span style="cursor: pointer">{{ scope.row.producedQuantity }}</span>
              </template>
            </el-table-column>
            <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`*长度\n(mm)`" align="left" min-width="85">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add || editing"
                  v-model.number="scope.row.length"
                  :min="0"
                  :max="maxNubmer"
                  :step="1"
                  placeholder="请填写"
                  :precision="DP.MES_ARTIFACT_L__MM"
                  controls-position="right"
                  style="width: 90px"
                  size="mini"
                />
                <span v-else>{{ scope.row.length ? scope.row.length.toFixed(DP.MES_MACHINE_PART_L__MM) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="*材质" min-width="100">
              <template v-slot="scope">
                <el-input
                  v-if="scope.row.add || editing"
                  v-model="scope.row.material"
                  type="text"
                  placeholder="请填写材质"
                  style="width: 100px"
                  size="mini"
                />
                <span v-else>{{ scope.row.material }}</span>
              </template> </el-table-column
            >>
            <el-table-column key="netWeight" prop="netWeight" :label="`*单净重\n(kg)`" align="left" min-width="80">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add || editing"
                  v-model.number="scope.row.netWeight"
                  :min="0"
                  :max="maxNubmer"
                  :step="1"
                  :precision="DP.COM_WT__KG"
                  placeholder="请填写"
                  controls-position="right"
                  style="width: 80px"
                  size="mini"
                />
                <span v-else>{{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="grossWeight" prop="grossWeight" :label="`*单毛重\n(kg)`" align="left" min-width="80">
              <template v-slot="scope">
                <el-input-number
                  v-if="scope.row.add || editing"
                  v-model.number="scope.row.grossWeight"
                  :min="0"
                  :max="maxNubmer"
                  :step="1"
                  :precision="DP.COM_WT__KG"
                  placeholder="请填写"
                  controls-position="right"
                  style="width: 80px"
                  size="mini"
                />
                <span v-else>{{ scope.row.grossWeight ? scope.row.grossWeight.toFixed(DP.COM_WT__KG) : '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column key="shearType" prop="shearType" :show-overflow-tooltip="true" label="类型" align="center" width="100">
              <template v-slot="scope">
                <common-select
                  v-if="scope.row.add || editing"
                  v-model="scope.row.shearType"
                  :options="shearTypeEnum.ENUM"
                  type="enum"
                  size="mini"
                  clearable
                  placeholder="类型"
                  style="width: 100%"
                />
                <span v-else>{{ scope.row.shearType }}</span>
                <!-- <span v-else>{{ isNotBlank(scope.row.shearType)? shearTypeEnum.VL(scope.row.shearType) : '-' }}</span> -->
              </template>
            </el-table-column>
          </common-table>
        </div>
        <div class="item-name">其他信息</div>
        <el-form-item label="原因描述" prop="remark">
          <el-input
            v-model.trim="form.otherRemark"
            type="textarea"
            :autosize="{ minRows: 4, maxRows: 6 }"
            placeholder="请填写原因描述"
            style="width: 320px"
          />
        </el-form-item>
        <el-form-item label="附件上传" prop="files" style="position: relative">
          <!-- <upload-btn ref="upload" :files.sync="form.files" :file-classify="fileClassifyEnum.CHANGE_LIST_ATT.V" /> -->
          <common-button size="small" type="danger" plain style="position: absolute; top: 0; left: 160px" @click="resetUpload()"
            >重置</common-button
          >
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref, defineProps, watch, computed } from 'vue'
import { regForm } from '@compos/use-crud'
import IconSelect from '@comp/iconSelect/index.vue'
import { isNotBlank } from '@data-type/index'
import { shearTypeEnum } from '@enum-ms/plan'
import { DP } from '@/settings/config'
import useTableValidate from '@compos/form/use-table-validate'
// import UploadBtn from '@/components/FileUpload/UploadBtn'

const formRef = ref()
const editing = ref(false)
const originData = ref([])
const isdisable = ref(false)
const maxNubmer = 999999999
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined,
  },
})
const defaultForm = {
  id: undefined,
  name: '',
  specification: '',
  material: '',
  // quantity: null,
  netWeight: undefined,
  grossWeight: undefined,
  drawingNumber: '',
  area: undefined,
  length: undefined,
  children: [],
  remark: '',
  otherRemark: '',
  files: [],
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const checkOtherDate = (rule, value, callback) => {
  if (!value) {
    callback(new Error('请选择完成时间'))
  } else {
    if (crud.form.productType && props.typeInfo && props.typeInfo.length > 0) {
      const val = props.typeInfo.find((v) => v.no === crud.form.productType)
      if (value > val.date) {
        callback(new Error('不能超过产品类型完成时间'))
      } else {
        callback()
      }
    }
  }
}
const rules = {
  name: [
    { required: true, message: '请填写构件名称', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  serialNumber: [
    { required: true, message: '请填写构件编号', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  specification: [
    { required: true, message: '请填写构件规格', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  material: [
    { required: true, message: '请填写构件材质', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' },
  ],
  drawingNumber: [{ max: 64, message: '不能超过64个字符', trigger: 'blur' }],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }],
  length: [{ required: true, message: '请填写构件长度', trigger: 'blur', type: 'number' }],
  netWeight: [{ required: true, message: '请填写构件净重', trigger: 'blur', type: 'number' }],
  grossWeight: [{ required: true, message: '请填写构件毛重', trigger: 'blur', type: 'number' }],
  area: [{ message: '请填写构件面积', trigger: 'blur', type: 'number' }],
}

const tableRules = {
  serialNumber: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  specification: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  quantity: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  // producedQuantity: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  length: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  material: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  grossWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
  netWeight: [{ required: true, max: 50, message: '不能超过 50 个字符', trigger: 'blur' }],
}
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

function handleAdd() {
  originData.value = JSON.parse(JSON.stringify(crud.form.children))
  crud.form.children.unshift({
    areaId: undefined,
    drawingNumber: undefined,
    grossWeight: undefined,
    length: undefined,
    matched: undefined,
    material: undefined,
    netWeight: undefined,
    outboundQuantity: undefined,
    producedQuantity: undefined,
    quantity: undefined,
    remark: '',
    reuseIngQuantity: undefined,
    scrappedQuantity: undefined,
    serialNumber: '',
    shearType: undefined,
    specification: '',
    status: undefined,
    add: true,
  })
  isdisable.value = true
}
function handleEdit() {
  editing.value = true
  originData.value = JSON.parse(JSON.stringify(crud.form.children))
  isdisable.value = true
}
function closeEdit() {
  editing.value = false
  originData.value.map((v) => {
    v.add = false
  })
  crud.form.children = originData.value
  isdisable.value = false
}
function saveEdit() {
  const { validResult, dealList } = tableValidate(crud.form.children)
  if (validResult) {
    crud.form.children = dealList
  } else {
    return validResult
  }
  editing.value = false
  originData.value.map((v) => {
    v.add = false
  })
  crud.form.children.map((v) => {
    v.add = false
  })
  isdisable.value = false
}

function handleSelectionChange(val) {}

function resetUpload() {}

CRUD.HOOK.afterToAdd = (crud, form) => {
  crud.form.productType = crud.query.productType
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.monomerId = crud.query.monomerId
  return !!crud.form.monomerId
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
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.table-form {
  ::v-deep(.el-input__inner) {
    padding: 0;
    padding-left: 5px;
  }
}
</style>

