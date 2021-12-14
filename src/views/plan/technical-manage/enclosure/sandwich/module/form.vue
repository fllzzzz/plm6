<template>
  <common-drawer
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    :wrapper-closable="false"
    size="860px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px">
        <div v-for="(content,index) in keyData" :key="index" style="margin-top:10px;">
          <div class="item-name">{{ content.type }}</div>
          <div class="div-content">
            <template v-if="content.type!='内板' && content.type!='外板'">
              <el-form-item
                v-for="(item,index2) in content.fields"
                :key="item.field+index2"
                :prop="item.field"
                :label="item.name"
              >
                <template v-if="!item.disabled">
                  <el-input
                    v-if="item.type==='text'"
                    v-model="form[item.field]"
                    type="text"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    style="width: 270px;"
                  />
                  <el-input-number
                    v-else
                    v-model.number="form[item.field]"
                    :min="0"
                    :max="maxNubmer"
                    :step="1"
                    :precision="item.decimalPlace"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    controls-position="right"
                    style="width: 270px;"
                  />
                </template>
                <template v-else>
                  <span>{{ form[item.field] }}</span>
                </template>
              </el-form-item>
            </template>
            <template v-if="content.type=='外板'">
              <el-form-item
                v-for="(item,index2) in content.fields"
                :key="item.field+index2"
                :prop="'outboard.'+item.field"
                :label="item.name"
              >
                <template v-if="!item.disabled">
                  <el-input
                    v-if="item.type==='text'"
                    v-model="form.outboard[item.field]"
                    type="text"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    style="width: 270px;"
                  />
                  <el-input-number
                    v-else
                    v-model.number="form.outboard[item.field]"
                    :min="0"
                    :max="maxNubmer"
                    :step="1"
                    :precision="item.decimalPlace"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    controls-position="right"
                    style="width: 270px;"
                  />
                </template>
                <template v-else>
                  <span>{{ form.outboard[item.field] }}</span>
                </template>
              </el-form-item>
            </template>
            <template v-if="content.type=='内板'">
              <el-form-item
                v-for="(item,index2) in content.fields"
                :key="item.field+index2"
                :prop="'inboard.'+item.field"
                :label="item.name"
              >
                <template v-if="!item.disabled">
                  <el-input
                    v-if="item.type==='text'"
                    v-model="form.inboard[item.field]"
                    type="text"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    style="width: 270px;"
                  />
                  <el-input-number
                    v-else
                    v-model.number="form.inboard[item.field]"
                    :min="0"
                    :max="maxNubmer"
                    :step="1"
                    :precision="item.decimalPlace"
                    :placeholder="item.name+ `${item.unit?'，单位：'+item.unit:''}`"
                    controls-position="right"
                    style="width: 270px;"
                  />
                </template>
                <template v-else>
                  <span>{{ form.inboard[item.field] }}</span>
                </template>
              </el-form-item>
            </template>
          </div>
        </div>
        <el-form-item label="备注" prop="remark">
          <el-input
            v-model.trim="form.remark"
            type="textarea"
            :autosize="{ minRows: 1, maxRows: 6}"
            placeholder="请填写备注"
            style="width: 320px;"
          />
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
import { DP } from '@/settings/config'

const formRef = ref()
const isdisable = ref(false)
const maxNubmer = 999999999
const defaultForm = {
  id: undefined,
  name: '',
  plate: '',
  thickness: undefined,
  width: undefined,
  length: undefined,
  quantity: undefined,
  type: '',
  brand: '',
  capacity: undefined,
  sandwichColorBoardList: [],
  outboard: {
    boardShape: '',
    brand: '',
    thickness: undefined,
    width: undefined,
    coating: undefined,
    claddingMaterial: '',
    color: ''
  },
  inboard: {
    boardShape: '',
    brand: '',
    thickness: undefined,
    width: undefined,
    coating: undefined,
    claddingMaterial: '',
    color: ''
  },
  remark: ''
}
const { CRUD, crud, form } = regForm(defaultForm, formRef)

const validateLength = (message, length) => {
  return [
    { required: true, message: message, trigger: ['change', 'blur'] },
    { max: length, message: `长度在 ${length} 个字符以内`, trigger: ['change', 'blur'] }
  ]
}
const validateThickness = (rule, value, callback) => {
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,3}$/
  if (!value) {
    callback(new Error('请选择或填写厚度'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入数字且最多保留3位小数`))
  } else {
    callback()
  }
}

const validateWeight = (rule, value, callback) => {
  var reg = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,2}$/
  if (!value) {
    callback(new Error('请选择或填写容重'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入数字且最多保留2位小数`))
  } else {
    callback()
  }
}

const validateWidth = (rule, value, callback) => {
  var reg = /^[+]{0,1}(\d+)$/
  if (!value) {
    callback(new Error('请选择或填写有效宽度'))
  } else if (!reg.test(value)) {
    callback(new Error(`请输入整数`))
  } else {
    callback()
  }
}

const keyData=[
  {
    type: '产品信息',
    fields: [
      { field: 'name', name: '名称', dict: 'brand', placeholder: '请填写名称', rules: validateLength('请填写品牌', 20), disabled: true },
      { field: 'plate', name: '板型', dict: 'model', placeholder: '请填写板型', rules: validateLength('请填写板型', 10), type: 'text' },
      { field: 'thickness', name: '厚度', dict: 'thickness', placeholder: '请填写厚度', rules: [
        { validator: validateThickness, trigger: ['change', 'blur'] },
        { required: true, message: '请填写厚度', trigger: ['change', 'blur'] }
      ], unit: 'mm', decimalPlace: 3, type: 'number' },
      { field: 'width', name: '有效宽度(mm)', dict: 'effective_width', placeholder: '请填写有效宽度', rules: [
        { validator: validateWidth, trigger: ['change', 'blur'] },
        { required: true, message: '请填写有效宽度', trigger: ['change', 'blur'] }
      ], decimalPlace: 0, type: 'number' },
      { field: 'length', name: '单长(mm)', dict: 'effective_width', placeholder: '请填写单长', rules: [
        { validator: validateWidth, trigger: ['change', 'blur'] },
        { required: true, message: '请填写单长', trigger: ['change', 'blur'] }
      ], decimalPlace: 0, type: 'number' },
      { field: 'quantity', name: '数量(张)', dict: 'quantity', placeholder: '请填写数量', rules: [
        { validator: validateWidth, trigger: ['change', 'blur'] },
        { required: true, message: '请填写数量', trigger: ['change', 'blur'] }
      ], decimalPlace: 0, type: 'number' }
    ]
  },
  {
    type: '外板',
    fields: [
      { field: 'boardShape', name: '板形状', dict: 'form_inout', placeholder: '请填写板形状', rules: validateLength('请选择或填写板形状', 10), type: 'text' },
      { field: 'brand', name: '品牌', dict: 'brand_inout', placeholder: '请填写品牌', rules: validateLength('请选择或填写品牌', 20), type: 'text' },
      { field: 'thickness', name: '厚度(mm)', dict: 'thickness_inout', placeholder: '请填写厚度', rules: [
        { validator: validateThickness, trigger: ['change', 'blur'] },
        { required: true, message: '请填写厚度', trigger: ['change', 'blur'] }
      ], decimalPlace: 3, type: 'number' },
      { field: 'width', name: '宽度(mm)', dict: 'width_inout', placeholder: '请填写宽度', rules: [
        { validator: validateWidth, trigger: ['change', 'blur'] },
        { required: true, message: '请填写宽度', trigger: ['change', 'blur'] }
      ], decimalPlace: 0, type: 'number' },
      { field: 'coating', name: '涂层', dict: 'coating_inout', placeholder: '请填写涂层', rules: validateLength('请选择或填写涂层', 10), type: 'text' },
      { field: 'claddingMaterial', name: '镀层', dict: 'cladding_inout', placeholder: '请填写镀层', rules: validateLength('请选择或填写镀层', 20), type: 'text' },
      { field: 'color', name: '颜色', dict: 'color_inout', placeholder: '请填写颜色', rules: validateLength('请选择或填写颜色', 10), type: 'text' }
    ]
  },
  {
    type: '内板',
    fields: [
      { field: 'boardShape', name: '板形状', dict: 'form_inout', placeholder: '请填写板形状', rules: validateLength('请选择或填写板形状', 10), type: 'text' },
      { field: 'brand', name: '品牌', dict: 'brand_inout', placeholder: '请填写品牌', rules: validateLength('请选择或填写品牌', 20), type: 'text' },
      { field: 'thickness', name: '厚度(mm)', dict: 'thickness_inout', placeholder: '请填写厚度', rules: [
        { validator: validateThickness, trigger: ['change', 'blur'] },
        { required: true, message: '请填写厚度', trigger: ['change', 'blur'] }
      ], decimalPlace: 3, type: 'number' },
      { field: 'width', name: '宽度(mm)', dict: 'width_inout', placeholder: '请填写宽度', rules: [
        { validator: validateWidth, trigger: ['change', 'blur'] },
        { required: true, message: '请填写宽度', trigger: ['change', 'blur'] }
      ], decimalPlace: 0, type: 'number' },
      { field: 'coating', name: '涂层', dict: 'coating_inout', placeholder: '请填写涂层', rules: validateLength('请选择或填写涂层', 10), type: 'text' },
      { field: 'claddingMaterial', name: '镀层', dict: 'cladding_inout', placeholder: '请填写镀层', rules: validateLength('请选择或填写镀层', 20), type: 'text' },
      { field: 'color', name: '颜色', dict: 'color_inout', placeholder: '请填写颜色', rules: validateLength('请选择或填写颜色', 10), type: 'text' }
    ]
  },
  {
    type: '芯材',
    fields: [
      { field: 'type', name: '种类', dict: 'kind_core', placeholder: '请填写种类', rules: validateLength('请选择或填写种类', 10), type: 'text' },
      { field: 'brand', name: '品牌', dict: 'brand_core', placeholder: '请填写品牌', rules: validateLength('请选择或填写品牌', 20), type: 'text' },
      { field: 'capacity', name: '容重', dict: 'unit_weight_core', placeholder: '请填写容重', rules: [
        { validator: validateWeight, trigger: ['change', 'blur'] },
        { required: true, message: '请填写容重', trigger: ['change', 'blur'] }
      ], unit: 'Kg/m³', decimalPlace: 2, type: 'number' }
    ]
  }
]

const rules = {
  name: [
    { required: true, message: '请填写名称', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  serialNumber: [
    { required: true, message: '请填写编号', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  plate: [
    { required: true, message: '请填写板型', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  thickness: [{ required: true, message: '请填写厚度', trigger: 'blur', type: 'number' }],
  width: [{ required: true, message: '请填写有效宽度', trigger: 'blur', type: 'number' }],
  length: [{ required: true, message: '请填写长度', trigger: 'blur', type: 'number' }],
  quantity: [{ required: true, message: '请填写数量', trigger: 'blur', type: 'number' }],
  type: [
    { required: true, message: '请填写种类', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  brand: [
    { required: true, message: '请填写品牌', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  capacity: [{ required: true, message: '请填写容重', trigger: 'blur', type: 'number' }],
  remark: [{ max: 500, message: '不能超过 500 个字符', trigger: 'blur' }],
  'outboard.boardShape': [
    { required: true, message: '请填写板形状', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'outboard.brand': [
    { required: true, message: '请填写品牌', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'outboard.thickness': [{ required: true, message: '请填写厚度', trigger: 'blur', type: 'number' }],
  'outboard.width': [{ required: true, message: '请填写有效宽度', trigger: 'blur', type: 'number' }],
  'outboard.coating': [
    { required: true, message: '请填写涂层', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'outboard.claddingMaterial': [
    { required: true, message: '请填写镀层', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'outboard.color': [
    { required: true, message: '请填写颜色', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'inboard.boardShape': [
    { required: true, message: '请填写板形状', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'inboard.brand': [
    { required: true, message: '请填写品牌', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'inboard.thickness': [{ required: true, message: '请填写厚度', trigger: 'blur', type: 'number' }],
  'inboard.width': [{ required: true, message: '请填写有效宽度', trigger: 'blur', type: 'number' }],
  'inboard.coating': [
    { required: true, message: '请填写涂层', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'inboard.claddingMaterial': [
    { required: true, message: '请填写镀层', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ],
  'inboard.color': [
    { required: true, message: '请填写颜色', trigger: 'blur' },
    { min: 1, max: 64, message: '长度在 1 到 64 个字符', trigger: 'blur' }
  ]
}

CRUD.HOOK.beforeSubmit = (crud, form) => {
  crud.form.totalArea = (crud.form.width * crud.form.length * crud.form.quantity) / 1000000
  crud.form.totalLength = (crud.form.length * crud.form.quantity) / 1000
  crud.form.sandwichColorBoardList[0] = crud.form.inboard
  crud.form.sandwichColorBoardList[1] = crud.form.outboard
}
</script>
<style rel="stylesheet/scss" lang="scss" scoped>
  ::v-deep(.el-input-number .el-input__inner) {
    text-align: left;
  }
  ::v-deep(.el-dialog__body){
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
  .div-content{
    display:flex;
    flex-wrap:wrap;
    ::v-deep(.el-form-item){
      width:50%;
    }
  }
  .item-name{
    padding: 8px 16px;
    background-color: #ecf8ff;
    border-radius: 4px;
    border-left: 5px solid #50bfff;
    margin: 10px 0;
    margin-left:5px;
    width: 150px;
  }
</style>

