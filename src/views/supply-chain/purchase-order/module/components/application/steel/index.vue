<template>
  <component ref="steelRef" :is="comp" v-bind="$attrs" />
</template>

<script setup>
import { ref, computed, reactive, inject, nextTick, watch, defineExpose, watchEffect } from 'vue'
import { steelClsEnum } from '@enum-ms/classification'

import { isNotBlank, isBlank, toPrecision } from '@/utils/data-type'

import SteelPlateTable from './module/steel-plate-table'
import SectionSteelTable from './module/section-steel-table'
import SteelCoilTable from './module/steel-coil-table'
import { ElMessage } from 'element-plus'
import { DP } from '@/settings/config'

const form = inject('cu')?.form
const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const steelRef = ref()

watchEffect(() => {
  let _mete = 0
  let _amount = 0
  const list = ['steelPlateList', 'sectionSteelList', 'steelCoilList']
  list.forEach((key) => {
    if (isNotBlank(form[key])) {
      form[key].forEach((v) => {
        _mete += v.weighingTotalWeight || 0
        _amount += Number(v.amount) || 0
      })
    }
  })
  form.amount = toPrecision(_amount, DP.YUAN)
  form.mete = _mete
  form.meteUnit = 'kg'
})

// 钢材三个组件的ref列表
const steelRefList = reactive({
  steelPlateList: null,
  sectionSteelList: null,
  steelCoilList: null
})

// 使用草稿时，为数据设置监听
const setFormCallback = (form) => {
  const trigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const initSelectedTrigger = {
    steelPlateList: null,
    sectionSteelList: null,
    steelCoilList: null
  }
  const list = ['steelPlateList', 'sectionSteelList', 'steelCoilList']
  list.forEach((key) => {
    if (isNotBlank(form[key])) {
      form[key] = form[key].map((v) => reactive(v))
      trigger[key] = watch(
        steelRefList,
        (ref) => {
          if (ref[key]) {
            // 初始化选中数据，执行一次后取消当前监听
            initSelectedTrigger[key] = watch(
              matSpecRef,
              () => {
                // 初始化数据监听，执行一次后取消当前监听
                form[key].forEach((v) => ref[key]?.rowWatch(v))
                if (matSpecRef.value) {
                  matSpecRef.value.initSelected(
                    form[key].map((v) => {
                      return { sn: v.sn, classifyId: v.classifyId }
                    })
                  )
                  nextTick(() => {
                    initSelectedTrigger[key]()
                  })
                }
              },
              { immediate: true }
            )
            nextTick(() => {
              trigger[key]()
            })
          }
        },
        { immediate: true, deep: true }
      )
    }
  })
}

// 监听切换钢材类型，为list赋值
watch(
  () => form.currentBasicClass,
  (v) => {
    if (v) {
      nextTick(() => {
        // nextTick 后 steelRef.value 才会发生变化
        let k = ''
        switch (v) {
          case steelClsEnum.STEEL_PLATE.V:
            k = 'steelPlateList'
            break
          case steelClsEnum.SECTION_STEEL.V:
            k = 'sectionSteelList'
            break
          case steelClsEnum.STEEL_COIL.V:
            k = 'steelCoilList'
            break
          default:
            break
        }
        if (!steelRefList[k]) steelRefList[k] = steelRef.value
        // 初始化数据监听，执行一次后取消当前监听
        form[k]?.forEach((v) => ref[k]?.rowWatch(v))
      })
    }
  },
  { immediate: true }
)

const comp = computed(() => {
  switch (form.currentBasicClass) {
    case steelClsEnum.STEEL_PLATE.V:
      return SteelPlateTable
    case steelClsEnum.SECTION_STEEL.V:
      return SectionSteelTable
    case steelClsEnum.STEEL_COIL.V:
      return SteelCoilTable
    default:
      return ''
  }
})

function rowInit(row) {
  return steelRef.value?.rowInit(row)
}

// 表单校验
function validate() {
  if (isBlank(form.steelPlateList) && isBlank(form.sectionSteelList) && isBlank(form.steelCoilList)) {
    ElMessage.warning('请填写数据')
    return false
  }
  const tableValidateRes = validateTable()
  if (tableValidateRes) {
    form.list = [...form.steelPlateList, ...form.sectionSteelList, ...form.steelCoilList]
    form.basicClass = form.currentBasicClass
    form.list.forEach((v) => {
      form.basicClass = form.basicClass | v.basicClass
      v.mete = v.weighingTotalWeight
      v.weight = v.weighingTotalWeight
    })
  }
  // 进入汇总页面
  return tableValidateRes
}

// 表格校验
function validateTable() {
  return Object.keys(steelRefList).every((k) => (steelRefList[k] ? steelRefList[k].validate() : true))
}

function rowWatch(row) {
  steelRef.value?.rowWatch(row)
}

defineExpose({
  rowInit,
  validate,
  rowWatch,
  setFormCallback
})
</script>

<style lang="scss" scoped></style>
