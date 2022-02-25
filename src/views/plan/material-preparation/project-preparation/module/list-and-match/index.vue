<template>
  <div>
    <div class="list-container" :style="{ height: props.height + 'px' }">
      <component v-if="detail.listType" v-loading="crud.editDetailLoading" :is="listComp" :list="list" />
    </div>
    <div class="match-container" :style="{ height: props.height + 'px' }">
      <!-- <component v-if="" /> -->
    </div>
  </div>
</template>

<script setup>
import { ref, computed, defineProps } from 'vue'
import { componentTypeEnum } from '@enum-ms/building-steel'
import { regExtra } from '@compos/use-crud'
import { deepClone } from '@/utils/data-type'

import StructureList from './structure'

const props = defineProps({
  height: {
    type: Number,
    default: 600
  }
})

// 技术清单汇总列表
const list = ref([])
// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()
const detail = crud.form

const listComp = computed(() => {
  switch (detail.listType) {
    case componentTypeEnum.STRUCTURE.V:
      return StructureList // 构件技术清单
    case componentTypeEnum.ENCLOSURE.V:
      return StructureList // 围护技术清单
    case componentTypeEnum.AUXILIARY_MATERIAL.V:
      return StructureList // 辅材技术清单
    default:
      return null
  }
})

CRUD.HOOK.beforeEditDetailLoaded = (crud, form) => {
  // 设置技术清单汇总
  list.value = deepClone(form.technologyList || [])
}

</script>
