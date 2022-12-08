<template>
  <common-drawer
    ref="drawerRef"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    :title="showType === 'edit'?'编辑构件特征定义':'新增构件特征定义'"
    :center="false"
    :close-on-click-modal="false"
    size="800px"
  >
    <template #titleRight>
      <common-button :loading="loading" type="primary" size="mini" @click="onSubmit">确认</common-button>
    </template>
    <template #content>
      <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="140px" :max-height="maxHeight">
        <el-form-item label="生产线" prop="productionLineTypeEnum">
          <common-select
            v-model="form.productionLineTypeEnum"
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
        <el-form-item label="构件类型" prop="artifactType" v-if="form.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V">
          <common-select
            v-model="form.artifactType"
            :options="artifactTypeEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            placeholder="构件类型"
            style="width: 250px"
            @change="artifactTypeChange"
            :disabled="!!form.id"
          />
        </el-form-item>
        <el-form-item label="类型" prop="parentType" v-if="form.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
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
        <el-form-item label="类型命名" prop="classificationName" v-if="form.productionLineTypeEnum">
          <common-select
            v-if="form.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V"
            v-model="form.classificationName"
            :options="classificationNameOptions"
            type="other"
            :dataStructure="{ key: 'key', label: 'value', value: 'value' }"
            size="small"
            class="filter-item"
            placeholder="类型命名选择"
            style="width: 250px"
          />
          <el-input v-model="form.classificationName" type="text" placeholder="类型命名" style="width: 250px" maxlength="30" v-else />
        </el-form-item>
        <!-- <el-form-item label="长度定义(mm)" prop="minLength" v-if="form.parentType === intellectParentType.BRIDGE.V">
          <div style="margin-bottom:5px;">类型为梁时,最小值和最大值至少填一项</div>
            <span style="margin-right:3px;">最小值:</span>
            <common-input-number
              v-model="form.minLength"
              :min="0"
              :max="999999999"
              :controls="false"
              :step="1"
              :precision="0"
              size="mini"
              placeholder="最小值"
              style="width:100px;margin-right:5px;line-height:32px !import;"
            />
            <span style="padding:0 8px;">—</span>
            <span style="margin-right:3px;">最大值:</span>
            <common-input-number
              v-model="form.maxLength"
              :min="0"
              :max="999999999"
              :controls="false"
              :step="1"
              :precision="0"
              size="mini"
              placeholder="最大值"
              style="width:100px;line-height:32px;"
            />
        </el-form-item> -->
        <el-form-item label="定义代码" prop="definitionWord" v-if="form.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V">
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
            <common-button icon="el-icon-plus" size="mini" type="success" style="margin: 0 0 12px 6px" @click="addProcess" v-if="plusShow"/>
          </div>
        </el-form-item>
        <el-form-item label="编号类型索引" prop="serialNumberPrefixList" v-if="form.artifactType===artifactTypeEnum.SMALL.V">
          <div class="process-container">
            <div class="process-box">
              <div v-for="(item, index) in form.serialNumberPrefixList" :key="index" class="process-drawer">
                <el-input
                  v-model="item.serialNumberPrefix"
                  type="text"
                  placeholder="编号类型索引"
                  style="width: 270px; margin-right: 5px"
                  @blur="checkSerialNumber(item, index)"
                />
                <common-button
                  v-show="form.serialNumberPrefixList	 && form.serialNumberPrefixList.length > 1"
                  icon="el-icon-delete"
                  size="mini"
                  type="danger"
                  style="margin-left: 6px"
                  @click="delSerialNumber(index)"
                />
              </div>
            </div>
            <common-button icon="el-icon-plus" size="mini" type="success" style="margin: 0 0 12px 6px" @click="addSerialNumber" />
          </div>
        </el-form-item>
        <el-form-item label="打码方式" prop="codingType" v-if="form.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V">
          <common-select
            v-model="form.codingType"
            :options="codingTypeEnum.ENUM"
            type="enum"
            size="small"
            class="filter-item"
            placeholder="打码方式"
            style="width: 250px"
          />
        </el-form-item>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import crudApi from '@/api/config/system-config/artifact-config'
import { defineProps, defineEmits, ref, watch, nextTick, computed } from 'vue'
import { ElMessage, ElNotification } from 'element-plus'

// import { isNotBlank } from '@data-type/index'
// import { whetherEnum } from '@enum-ms/common'
import { artifactProductLineEnum, intellectParentType, artifactTypeEnum, codingTypeEnum } from '@enum-ms/mes'

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
const serialNumberArr = ref([])
const defaultForm = {
  id: undefined,
  productionLineTypeEnum: undefined,
  parentType: undefined, // 类型选择
  artifactType: undefined, // 构件类型
  classificationName: '', // 类型命名
  // boolContainsMin: undefined, // 最小值是否包含等于
  // minLength: undefined, // 长度最小值
  // boolContainsMax: undefined, // 最大值是否包含等于
  // maxLength: undefined, // 长度最大值
  definitionWord: undefined, // 定义代码
  sort: undefined,
  specPrefixList: [],
  serialNumberPrefixList: [],
  codingType: undefined
}

const form = ref(JSON.parse(JSON.stringify(defaultForm)))
const plusShow = computed(() => {
  return form.value.productionLineTypeEnum === artifactProductLineEnum.TRADITION.V ? (form.value.specPrefixList?.length < 1 ? true : (form.value.artifactType === artifactTypeEnum.COMMON.V)) : true
})

const classificationNameOptions = computed(() => {
  let options = []
  if (form.value.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V && form.value.parentType) {
    options = form.value.parentType === intellectParentType.PILLAR.V ? [{ key: 1, value: '钢柱' }] : [{ key: 2, value: '钢梁' }, { key: 3, value: '短梁' }, { key: 4, value: '长短梁' }]
  }
  return options
})

const validateLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].add) {
        if (!value[i].specPrefix) {
          callback(new Error('请填写大写关键字母'))
        }
        // if (!isNotBlank(value[i].boolUseAssemble)) {
        //   callback(new Error('请选择是否匹配部件'))
        // }
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写大写规格前缀'))
  }
}

const validateSerialNumberLinks = (rule, value, callback) => {
  if (value && value.length) {
    for (const i in value) {
      if (!value[i].add) {
        if (!value[i].serialNumberPrefix) {
          callback(new Error('请填写编号前缀'))
        }
      } else {
        callback()
      }
    }
    callback()
  } else {
    callback(new Error('请填写编号前缀'))
  }
}

// const validateLength = (rule, value, callback) => {
//   if (form.value.parentType === intellectParentType.BRIDGE.V) {
//     if (!form.value.minLength && !form.value.maxLength) {
//       callback(new Error('最大值和最小值不能同时为空'))
//     } else {
//       if (form.value.minLength && form.value.maxLength) {
//         if (form.value.maxLength < form.value.minLength) {
//           callback(new Error('最大值必须大于最小值'))
//         }
//         callback()
//       }
//     }
//   }
//   callback()
// }
const validateParentType = (rule, value, callback) => {
  if (form.value.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
    if (!value) {
      callback(new Error('请选择类型'))
    }
    callback()
  }
  callback()
}

const validateDefinitionWord = (rule, value, callback) => {
  if (form.value.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
    if (!value) {
      callback(new Error('请填写定义代码'))
    }
    callback()
  }
  callback()
}

const rules = {
  productionLineTypeEnum: [
    { required: true, message: '请选择生产线', trigger: 'change' }
  ],
  artifactType: [
    { required: true, message: '请选择构件类型', trigger: 'change' }
  ],
  parentType: [
    { required: true, validator: validateParentType, message: '请选择类型', trigger: 'change' }
  ],
  // minLength: [
  //   { required: true, validator: validateLength, trigger: 'change' }
  // ],
  classificationName: [
    { required: true, message: '请填写类型命名', trigger: ['blur', 'change'] },
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
  ],
  serialNumberPrefixList: [
    { required: true, message: '请填写编号前缀' },
    { validator: validateSerialNumberLinks, trigger: 'change' }
  ],
  codingType: [
    { required: true, message: '请选择打码方式', trigger: 'change' }
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
    form.value = JSON.parse(JSON.stringify(data))
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
  form.value.classificationName = undefined
  if (val !== artifactProductLineEnum.INTELLECT.V) {
    // form.value.boolContainsMin = undefined
    // form.value.minLength = undefined
    // form.value.boolContainsMax = undefined
    // form.value.maxLength = undefined
    form.value.parentType = undefined
    form.value.definitionWord = undefined
  } else {
    form.value.serialNumberPrefixList = []
    form.value.artifactType = undefined
    form.value.codingType = undefined
  }
}

function parentTypeChange(val) {
  form.value.classificationName = undefined
}

function artifactTypeChange(val) {
  if (val !== artifactTypeEnum.SMALL.V) {
    form.value.serialNumberPrefixList = []
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

function addSerialNumber() {
  form.value.serialNumberPrefixList.push({
    add: true
  })
}
function delSerialNumber(index) {
  form.value.serialNumberPrefixList.splice(index, 1)
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

function checkSerialNumber(item, index) {
  item.add = false
  const val = serialNumberArr.value.find((v) => v.index === index)
  if (val) {
    if (item.serialNumberPrefix) {
      if (val.serialNumberPrefix === item.serialNumberPrefix) {
        return
      }
      if (serialNumberArr.value.findIndex((v) => v.serialNumberPrefix === item.serialNumberPrefix) > -1) {
        ElMessage({
          message: '编号前缀已存在，请重新填写',
          type: 'error'
        })
        item.serialNumberPrefix = undefined
        val.serialNumberPrefix = undefined
      } else {
        val.serialNumberPrefix = item.serialNumberPrefix
      }
    } else {
      val.serialNumberPrefix = undefined
    }
  } else {
    if (item.serialNumberPrefix) {
      if (serialNumberArr.value.findIndex((v) => v.serialNumberPrefix === item.serialNumberPrefix) > -1) {
        ElMessage({
          message: '编号前缀已存在，请重新填写',
          type: 'error'
        })
        form.value.serialNumberPrefixList[index].serialNumberPrefix = undefined
      }
      serialNumberArr.value.push({
        serialNumberPrefix: item.serialNumberPrefix,
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
  if (form.value.serialNumberPrefixList?.length) {
    form.value.serialNumberPrefixList.map((v) => {
      v.add = false
    })
  }
  if (form.value.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V) {
    form.value.artifactType = artifactTypeEnum.COMMON.V
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
    console.log('提交失败', error)
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
