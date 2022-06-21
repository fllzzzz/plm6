<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="showType === 'edit'?'编辑构件类型配置':'新增构件类型配置'"
    :center="false"
    :close-on-click-modal="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="loading" type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" :max-height="maxHeight">
        <el-form-item label="生产线" prop="productionLineType">
          <common-select
            v-model="form.productionLineType"
            :options="artifactProductLineEnum.ENUM"
            type="enum"
            size="small"
            clearable
            class="filter-item"
            placeholder="生产线"
            style="width: 250px"
            @change="lineTypeChange"
            :disabled="!!form.id"
          />
        </el-form-item>
        <el-form-item label="类型" prop="parentType" v-if="form.productionLineType === artifactProductLineEnum.INTELLECT.V">
          <common-select
            v-model="form.parentType"
            :options="intellectParentType.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            placeholder="类型选择"
            style="width: 250px"
            @change="parentTypeChange"
          />
        </el-form-item>
        <el-form-item label="类型命名" prop="classificationName">
          <el-input v-model="form.classificationName" type="text" placeholder="类型命名" style="width: 250px" maxlength="30" />
        </el-form-item>
        <el-form-item label="长度定义(mm)" prop="minLength" v-if="form.parentType === intellectParentType.BRIDGE.V">
          <div style="margin-bottom:5px;">类型为梁时,最小值和最大值至少填一项</div>
          <div style="margin-bottom:5px;">
            <span style="margin-right:3px;">最小值：</span>
            <common-select
              v-model="form.boolContainsMin"
              :options="minEqualTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              class="filter-item"
              placeholder="符号"
              style="width: 80px;margin-right:5px;"
            />
             <common-input-number
              v-model="form.minLength"
              :min="0"
              :max="999999999"
              :controls="false"
              :step="1"
              :precision="0"
              size="mini"
              placeholder="最小值"
              style="width:100px;"
            />
          </div>
          <div>
            <span style="margin-right:3px;">最大值：</span>
            <common-select
              v-model="form.boolContainsMax"
              :options="maxEqualTypeEnum.ENUM"
              type="enum"
              size="small"
              clearable
              class="filter-item"
              placeholder="符号"
              style="width: 80px;margin-right:5px;"
            />
            <common-input-number
              v-model="form.maxLength"
              :min="0"
              :max="999999999"
              :controls="false"
              :step="1"
              :precision="0"
              size="mini"
              placeholder="最大值"
              style="width:100px;"
            />
          </div>
        </el-form-item>
        <el-form-item label="定义代码" prop="definitionWord" v-if="form.productionLineType === artifactProductLineEnum.INTELLECT.V">
          <el-input v-model="form.definitionWord" type="text" placeholder="定义代码" style="width: 250px" maxlength="30" />
        </el-form-item>
        <el-form-item label="排序" prop="sort">
          <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
        </el-form-item>
        <el-form-item label="构件规格前缀" prop="specPrefixList">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.specPrefixList" :key="index" class="process-drawer">
                <el-input
                  v-model="item.specPrefix"
                  type="text"
                  placeholder="大写字母"
                  style="width: 270px; margin-right: 5px"
                  @blur="checkName(item, index)"
                />
                <common-select
                  v-model="item.boolUseAssemble"
                  :options="whetherEnum.ENUM"
                  type="enum"
                  size="small"
                  clearable
                  class="filter-item"
                  placeholder="是否匹配组立"
                  style="width: 250px"
                  @change="item.add = false"
                />
                <common-button
                  v-show="form.specPrefixList && form.specPrefixList.length > 1"
                  icon="el-icon-delete"
                  size="mini"
                  type="danger"
                  style="margin-left: 6px"
                  @click="delProcess(index)"
                />
              </div>
            </div>
            <common-button icon="el-icon-plus" size="mini" type="success" style="margin: 0 0 12px 6px" @click="addProcess" />
          </div>
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi from '@/api/config/system-config/artifact-config'
import { defineProps, defineEmits, ref, watch, nextTick } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

import { isNotBlank } from '@data-type/index'
import { whetherEnum } from '@enum-ms/common'
import { artifactProductLineEnum, intellectParentType, minEqualTypeEnum, maxEqualTypeEnum } from '@enum-ms/mes'

import useVisible from '@compos/use-visible'
import useWatchFormValidate from '@compos/form/use-watch-form-validate'
import useMaxHeight from '@compos/use-max-height'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  showType: {
    type: String,
    default: 'detail'
  },
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const formRef = ref()
const drawerRef = ref()
const loading = ref(false)
const nameArr = ref([])

const defaultForm = {
  id: undefined,
  productionLineType: undefined,
  parentType: undefined, // 类型选择
  classificationName: '', // 类型命名
  boolContainsMin: undefined, // 最小值是否包含等于
  minLength: undefined, // 长度最小值
  boolContainsMax: undefined, // 最大值是否包含等于
  maxLength: undefined, // 长度最大值
  definitionWord: undefined, // 定义代码
  sort: undefined,
  specPrefixList: []
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))

const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].add) {
        if (!value[i].specPrefix) {
          callback(new Error('请填写大写关键字母'))
        }
        if (!isNotBlank(value[i].boolUseAssemble)) {
          callback(new Error('请选择是否匹配组立'))
        }
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写大写规格前缀'))
  }
}

const validateLength = (rule, value, callback) => {
  if (form.value.parentType === intellectParentType.BRIDGE.V) {
    if (!form.value.minLength && !form.value.maxLength) {
      callback(new Error('最大值和最小值不能同时为空'))
    } else {
      if (form.value.minLength && form.value.maxLength) {
        if (!isNotBlank(form.value.boolContainsMin) || !isNotBlank(form.value.boolContainsMax)) {
          callback(new Error('最大值和最小值符号必选'))
        }
        if (form.value.maxLength < form.value.minLength) {
          callback(new Error('最大值必须大于最小值'))
        }
        callback()
      } else {
        if (form.value.minLength) {
          if (!isNotBlank(form.value.boolContainsMin)) {
            callback(new Error('最小值符号必选'))
          }
          callback()
        } else {
          if (!isNotBlank(form.value.boolContainsMax)) {
            callback(new Error('最大值符号必选'))
          }
          callback()
        }
      }
    }
  }
  callback()
}
const validateParentType = (rule, value, callback) => {
  if (form.value.productionLineType === artifactProductLineEnum.INTELLECT.V) {
    if (!value) {
      callback(new Error('请选择类型'))
    }
    callback()
  }
  callback()
}

const validateDefinitionWord = (rule, value, callback) => {
  if (form.value.productionLineType === artifactProductLineEnum.INTELLECT.V) {
    if (!value) {
      callback(new Error('请填写定义代码'))
    }
    callback()
  }
  callback()
}

const rules = {
  productionLineType: [
    { required: true, message: '请选择生产线', trigger: 'change' }
  ],
  parentType: [
    { required: true, validator: validateParentType, message: '请选择类型', trigger: 'change' }
  ],
  minLength: [
    { required: true, validator: validateLength, trigger: 'change' }
  ],
  classificationName: [
    { required: true, message: '请填写类型命名', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  definitionWord: [
    { required: true, validator: validateDefinitionWord, message: '请填写定义代码', trigger: 'blur' },
    { min: 1, max: 30, message: '长度在 1 到 30 个字符', trigger: 'blur' }
  ],
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  specPrefixList: [
    { required: true, message: '请填写规格前缀' },
    { validator: validateLinks, trigger: 'change' }
  ]
}

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.detail-header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 120
  },
  () => drawerRef.value.loaded
)

watch(
  () => visible.value,
  (val) => {
    if (val) {
      if (props.showType === 'edit') {
        resetForm(props.detailInfo)
      } else {
        resetForm()
      }
    }
  },
  { deep: true, immediate: true }
)

function resetForm(data) {
  nameArr.value = []
  if (formRef.value) {
    formRef.value.resetFields()
  }
  if (data && Object.keys(data).length > 0) {
    form.value = data
  } else {
    form.value = JSON.parse(JSON.stringify(defaultForm))
  }
  if (formRef.value) {
    nextTick(() => {
      formRef.value.clearValidate()
    })
  }
  useWatchFormValidate(formRef, form)
}

function lineTypeChange(val) {
  if (val !== artifactProductLineEnum.INTELLECT.V) {
    form.value.boolContainsMin = undefined
    form.value.minLength = undefined
    form.value.boolContainsMax = undefined
    form.value.maxLength = undefined
  }
}

function parentTypeChange(val) {
  if (val !== intellectParentType.BRIDGE.V) {
    form.value.boolContainsMin = undefined
    form.value.minLength = undefined
    form.value.boolContainsMax = undefined
    form.value.maxLength = undefined
  }
}

function addProcess() {
  form.value.specPrefixList.push({
    add: true
  })
}
function delProcess(index) {
  form.value.specPrefixList.splice(index, 1)
}

function checkName(item, index) {
  item.add = false
  const val = nameArr.value.find((v) => v.index === index)
  if (val) {
    if (item.specPrefix) {
      if (val.specPrefix === item.specPrefix) {
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        item.specPrefix = undefined
        val.specPrefix = undefined
      } else {
        if (!/^[A-Z]+$/.test(item.specPrefix)) {
          form.value.specPrefixList[index].specPrefix = undefined
          val.specPrefix = undefined
          return
        }
        val.specPrefix = item.specPrefix
      }
    } else {
      val.specPrefix = undefined
    }
  } else {
    if (item.specPrefix) {
      if (!/^[A-Z]+$/.test(item.specPrefix)) {
        form.value.specPrefixList[index].specPrefix = undefined
        return
      }
      if (nameArr.value.findIndex((v) => v.specPrefix === item.specPrefix) > -1) {
        ElMessage({
          message: '规格前缀已存在，请重新填写',
          type: 'error'
        })
        form.value.specPrefixList[index].specPrefix = undefined
      }
      nameArr.value.push({
        specPrefix: item.specPrefix,
        index: index
      })
    }
  }
}

async function onSubmit() {
  loading.value = true
  if (form.value.specPrefixList?.length) {
    form.value.specPrefixList.map((v) => {
      v.add = false
    })
  }
  try {
    const valid = await formRef.value.validate()
    if (!valid) {
      return
    }
    if (props.showType === 'edit') {
      await crudApi.edit(form.value)
    } else {
      await crudApi.add(form.value)
    }
    const msg = props.showType === 'edit' ? '修改成功' : '新增成功'
    ElNotification({ title: msg, type: 'success' })
    emit('success')
    handleClose()
  } catch (error) {
    console.log('价格修改失败', error)
  } finally {
    loading.value = false
  }
}

</script>

<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: flex-start;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
</style>
