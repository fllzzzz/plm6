<template>
  <span class="factory-table-cell-tag">
    <table-cell-tag v-if="cur" :name="cur.shortName" :color="cur.tagColor" />
  </span>
</template>

<script setup>
import { defineProps, computed } from 'vue'

import useFactory from '@compos/store/use-factories'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const props = defineProps({
  id: {
    type: Number
  }
})

const { factoryKV } = useFactory()

const cur = computed(() => {
  return factoryKV.value[props.id]
})

</script>
<style lang="scss" scoped>
.factory-table-cell-tag {
  margin-left: 15px;
}
</style>
