<template>
  <slot />
</template>
<!-- TODO:复用详情页面，后期优化 -->
<script setup>
import { defineProps, defineExpose } from 'vue'
import useCRUD from '@compos/use-crud'

const props = defineProps({
  api: {
    type: Function,
    required: true
  }
})

const { crud } = useCRUD(
  {
    crudApi: { detail: props.api },
    queryOnPresenterCreated: false // 不调用crud中的列表查询接口
  }
)

// 去详情 PS: 若后期有直接传入详情访问，自行改造
function toDetail(id) {
  crud.toDetail({ id })
}

defineExpose({
  toDetail
})
</script>
