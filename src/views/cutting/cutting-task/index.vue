<template>
  <div class="app-container">
    <div id="card-main-content" class="flex-rsc" style="padding-bottom: 20px; overflow-x: auto">
      <plant @selectionChange="selectionChange" :BooleanValue="queryBoolean" class="box-card" />
      <machine @query="query" :list="listData" class="machine" />
    </div>
  </div>
</template>

<script setup>
import plant from './module/plant.vue'
import machine from './module/machine.vue'
import { ref } from 'vue'

const listData = ref([])
const queryBoolean = ref(false)

function selectionChange(val) {
  if (val && val.length > 0) {
    listData.value = val.map((item) => { return item.id })
  } else {
    listData.value = []
  }
}

function query() {
  queryBoolean.value = !queryBoolean.value
}
</script>

<style lang="scss" scoped>
::v-deep(.card-header) {
  flex-wrap: wrap;
  .flex-rbc {
    width: 100%;
  }
}

.box-card {
  flex: 6.8;
  ::-webkit-scrollbar {
    width: 6px;
    height: 6px;
  }
  ::-webkit-scrollbar-thumb {
    border-radius: 6px;
  }
}
.machine {
  flex: 3.2;
  margin-left: 10px;
}

.app-container {
  padding: 20px;
}

.box-card + .box-card {
  margin-left: 10px;
}
</style>
